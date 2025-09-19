req <- c("here","readr","dplyr","ggplot2","tidyr","psych","gridExtra","stringr")
inst <- req[!req %in% installed.packages()[,1]]
if(length(inst)) install.packages(inst)
lapply(req, library, character.only = TRUE)

# Arquivos
dados_respostas <- read_csv(here("dados", "Opiniao dos estudantes (Responses).csv"))
dados_pesos <- read_csv(here("dados", "Peso das perguntas pesquisa.csv"))


# Exibe respostas obtidas
table(dados_respostas$`Qual a sua situação atual em relação ao ensino superior?`)
cat("Dim respostas (Pessoas/questoes):", dim(dados_respostas), "\n")

# Filtrar para manter apenas quem está cursando ou já concluiu
dados_respostas_filtrados <- dados_respostas %>%
  filter(`Qual a sua situação atual em relação ao ensino superior?` %in% 
           c("Estou cursando uma graduação/pós-graduação", 
             "Já concluí uma graduação/pós-graduação"))
# Exibe respostas filtradas
table(dados_respostas_filtrados$`Qual a sua situação atual em relação ao ensino superior?`)
cat("Respostas após filtro (Pessoas/questoes):", dim(dados_respostas_filtrados), "\n")


cat("Colunas respostas:\n"); print(names(dados_respostas_filtrados))
#cat("Dim pesos:", dim(dados_pesos), "\n")
#cat("Colunas pesos:\n"); print(names(dados_pesos))

# Seleção das colunas (F:J Likert = 6:10; K:O Thurstone = 11:15)
stopifnot(ncol(dados_respostas_filtrados) >= 15)
colunas_likert    <- dados_respostas_filtrados[, 6:10]
colunas_thurstone <- dados_respostas_filtrados[, 11:15]
names(colunas_likert)    <- paste0("Likert_", 1:5)
names(colunas_thurstone) <- paste0("Thurstone_", 1:5)

# Remover linhas totalmente vazias nas áreas F:O (comuns em CSV exportado)
zona_itens <- dados_respostas_filtrados[, 6:15]
linhas_validas <- apply(zona_itens, 1, function(r) any(!(is.na(r) | r == "")))
dados_respostas_filtrados <- dados_respostas_filtrados[linhas_validas, ]
colunas_likert    <- dados_respostas_filtrados[, 6:10]
colunas_thurstone <- dados_respostas_filtrados[, 11:15]
names(colunas_likert)    <- paste0("Likert_", 1:5)
names(colunas_thurstone) <- paste0("Thurstone_", 1:5)

# Conversão: Likert para numérico (robusto)
to_num_likert <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  # tenta conversão direta (ex.: "1","2")
  out <- suppressWarnings(as.numeric(x_chr))
  # se maioria virou NA, mapear textos
  if(mean(is.na(out)) > 0.5) {
    out <- dplyr::case_when(
      x_chr %in% c("1","discordo totalmente") ~ 1,
      x_chr %in% c("2","discordo parcialmente") ~ 2,
      x_chr %in% c("3","não concordo nem discordo") ~ 3,
      x_chr %in% c("4","concordo parcialmente") ~ 4,
      x_chr %in% c("5","concordo totalmente") ~ 5,
      str_detect(x_chr, "^1") ~ 1,
      str_detect(x_chr, "^2") ~ 2,
      str_detect(x_chr, "^3") ~ 3,
      str_detect(x_chr, "^4") ~ 4,
      str_detect(x_chr, "^5") ~ 5,
      TRUE ~ NA_real_
    )
  }
  out
}
colunas_likert_num <- as.data.frame(lapply(colunas_likert, to_num_likert))
names(colunas_likert_num) <- names(colunas_likert)

# Invertendo os itens 4 e 5 para valorização da prática (Perguntas pró-teoria para seguir mesma direção)
itens_invertidos <- c("Likert_4", "Likert_5") 
if(length(itens_invertidos)) {
  colunas_likert_num[itens_invertidos] <- lapply(colunas_likert_num[itens_invertidos], function(x) 6 - x)
}

# Escore Likert
dados_respostas_filtrados$escore_likert <- rowSums(colunas_likert_num, na.rm = TRUE)

itens_com_var <- colunas_likert_num[, sapply(colunas_likert_num, function(v) var(v, na.rm = TRUE) > 0), drop = FALSE]
if(ncol(itens_com_var) >= 2) {
  alpha_likert <- psych::alpha(itens_com_var, warnings = FALSE)
  cat("Alfa de Cronbach (Likert):", round(alpha_likert$total$raw_alpha, 3), "\n")
} else {
  alpha_likert <- NULL
  cat("Sem variabilidade suficiente nos itens Likert para calcular alfa.\n")
}

# Estatísticas Likert
likert_stats <- data.frame(
  Estatistica = c("Média","Mediana","Desvio Padrão","IQR","Min","Max"),
  Valor = c(
    mean(dados_respostas_filtrados$escore_likert, na.rm = TRUE),
    median(dados_respostas_filtrados$escore_likert, na.rm = TRUE),
    sd(dados_respostas_filtrados$escore_likert, na.rm = TRUE),
    IQR(dados_respostas_filtrados$escore_likert, na.rm = TRUE),
    min(dados_respostas_filtrados$escore_likert, na.rm = TRUE),
    max(dados_respostas_filtrados$escore_likert, na.rm = TRUE)
  )
)

# -------------------------------------------------------------------------------------------------
# 8) Thurstone: binariza pesos (0 discordo e 1 concordo)
to_bin_thurstone <- function(x){
  x_chr <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    x_chr == "concordo"  ~ 1,
    x_chr == "discordo" ~ 0,
    TRUE ~ NA_real_
  )
}
colunas_thurstone_bin <- as.data.frame(lapply(colunas_thurstone, to_bin_thurstone))
names(colunas_thurstone_bin) <- names(colunas_thurstone)

# Acessa os pesos e converte para numero
pesos_thurstone <- as.numeric(dados_pesos$`Mediana (peso final)`)
pesos_thurstone <- pesos_thurstone[1:5] 

# escore Thurstone = média dos pesos das afirmativas com concordo (1)
mat_bin <- as.matrix(colunas_thurstone_bin)
soma_pesos <- mat_bin %*% matrix(pesos_thurstone, ncol = 1)
qtd_conc   <- rowSums(colunas_thurstone_bin, na.rm = TRUE)

# Calcula escore thurstone e coloca na tabela respostasfiltradas (vlr 11-Valoriza teoria e 1-Valoriza pratica)
dados_respostas_filtrados$escore_thurstone <- ifelse(qtd_conc > 0, soma_pesos[,1] / qtd_conc, NA_real_)

# Estatísticas Thurstone
thurstone_stats <- data.frame(
  Estatistica = c("Média","Mediana","Desvio Padrão","IQR","Min","Max"),
  Valor = c(
    mean(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE),
    median(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE),
    sd(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE),
    IQR(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE),
    min(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE),
    max(dados_respostas_filtrados$escore_thurstone, na.rm = TRUE)
  )
)

cat("\nResumo Likert:\n"); print(likert_stats)
cat("\nResumo Thurstone:\n"); print(thurstone_stats)

# # 9) Gráficos
# dir.create("plots", showWarnings = FALSE)
# 
# g_hist_likert <- ggplot(dados_respostas_filtrados, aes(x = escore_likert)) +
#   geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.8) +
#   labs(title = "Distribuição de Escores - Likert", x = "Escore total", y = "Frequência") +
#   theme_minimal()
# 
# g_box_likert <- ggplot(dados_respostas_filtrados, aes(y = escore_likert)) +
#   geom_boxplot(fill = "skyblue") +
#   labs(title = "Boxplot - Likert", y = "Escore total") +
#   theme_minimal()
# 
# g_hist_thurst <- ggplot(dados_respostas_filtrados, aes(x = escore_thurstone)) +
#   geom_histogram(bins = 10, fill = "lightgreen", color = "black", alpha = 0.8) +
#   labs(title = "Distribuição de Escores - Thurstone", x = "Escore", y = "Frequência") +
#   theme_minimal()
# 
# g_box_thurst <- ggplot(dados_respostas_filtrados, aes(y = escore_thurstone)) +
#   geom_boxplot(fill = "lightgreen") +
#   labs(title = "Boxplot - Thurstone", y = "Escore") +
#   theme_minimal()
# 
# # Comparação e correlação
# dados_comp <- dados_respostas_filtrados %>%
#   select(escore_likert, escore_thurstone) %>%
#   pivot_longer(cols = everything(), names_to = "Escala", values_to = "Escore")
# 
# g_box_comp <- ggplot(dados_comp, aes(x = Escala, y = Escore, fill = Escala)) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("escore_likert" = "skyblue", "escore_thurstone" = "lightgreen")) +
#   labs(title = "Comparação de Escores", x = "", y = "Escore") +
#   theme_minimal()
# 
# cor_escalas <- suppressWarnings(cor(dados_respostas_filtrados$escore_likert,
#                                     dados_respostas_filtrados$escore_thurstone,
#                                     use = "complete.obs", method = "pearson"))
# cat("\nCorrelação Likert x Thurstone (pearson):", round(cor_escalas, 3), "\n")
# 
# g_scatter <- ggplot(dados_respostas_filtrados, aes(escore_likert, escore_thurstone)) +
#   geom_point(alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE, color = "red") +
#   labs(title = paste0("Correlação Likert x Thurstone (r = ", round(cor_escalas, 3), ")"),
#        x = "Escore Likert", y = "Escore Thurstone") +
#   theme_minimal()
# 
# # Salvar gráficos
# ggsave("plots/hist_likert.png", g_hist_likert, width = 6, height = 4, dpi = 150)
# ggsave("plots/box_likert.png", g_box_likert, width = 4, height = 4, dpi = 150)
# ggsave("plots/hist_thurstone.png", g_hist_thurst, width = 6, height = 4, dpi = 150)
# ggsave("plots/box_thurstone.png", g_box_thurst, width = 4, height = 4, dpi = 150)
# ggsave("plots/box_comparativo.png", g_box_comp, width = 5, height = 4, dpi = 150)
# ggsave("plots/scatter_correlacao.png", g_scatter, width = 5, height = 4, dpi = 150)
# 
# # 10) Exportar escores finais para uso no R Markdown
# saida <- dados_respostas_filtrados %>%
#   mutate(across(everything(), as.character)) %>%
#   bind_cols(
#     colunas_likert_num %>% mutate(across(everything(), as.character)),
#     colunas_thurstone %>% mutate(across(everything(), as.character))
#   ) %>%
#   select(escore_likert, escore_thurstone, everything())
# 
# write_csv(saida, "escores_finais.csv")
# cat("\nArquivo 'escores_finais.csv' salvo e gráficos na pasta 'plots/'.\n")