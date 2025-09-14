req <- c("readr","dplyr","ggplot2","tidyr","psych","gridExtra","stringr")
inst <- req[!req %in% installed.packages()[,1]]
if(length(inst)) install.packages(inst)
lapply(req, library, character.only = TRUE)

# 2) Ajuda: leitor automático de CSV (; ou ,)
read_csv_auto <- function(path) {
  stopifnot(file.exists(path))
  x <- try(readr::read_delim(path, delim = ";",
                             locale = locale(encoding = "UTF-8"),
                             show_col_types = FALSE), silent = TRUE)
  if(inherits(x, "try-error") || ncol(x) == 1) {
    x <- readr::read_delim(path, delim = ",",
                           locale = locale(encoding = "UTF-8"),
                           show_col_types = FALSE)
  }
  as.data.frame(x)
}

# Arquivos
arq_resp  <- "Opiniao dos estudantes (Responses).csv"
arq_pesos <- "Peso das perguntas pesquisa.csv"

dados_respostas <- read_csv_auto(arq_resp)
dados_pesos     <- read_csv_auto(arq_pesos)

cat("Dim respostas:", dim(dados_respostas), "\n")
cat("Colunas respostas:\n"); print(names(dados_respostas))
cat("Dim pesos:", dim(dados_pesos), "\n")
cat("Colunas pesos:\n"); print(names(dados_pesos))

# 4) Seleção das colunas (F:J Likert = 6:10; K:O Thurstone = 11:15)
stopifnot(ncol(dados_respostas) >= 15)
colunas_likert    <- dados_respostas[, 6:10]
colunas_thurstone <- dados_respostas[, 11:15]
names(colunas_likert)    <- paste0("Likert_", 1:5)
names(colunas_thurstone) <- paste0("Thurstone_", 1:5)

# 5) Remover linhas totalmente vazias nas áreas F:O (comuns em CSV exportado)
zona_itens <- dados_respostas[, 6:15]
linhas_validas <- apply(zona_itens, 1, function(r) any(!(is.na(r) | r == "")))
dados_respostas <- dados_respostas[linhas_validas, ]
colunas_likert    <- dados_respostas[, 6:10]
colunas_thurstone <- dados_respostas[, 11:15]
names(colunas_likert)    <- paste0("Likert_", 1:5)
names(colunas_thurstone) <- paste0("Thurstone_", 1:5)

# 6) Conversão: Likert para numérico (robusto)
to_num_likert <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  # tenta conversão direta (ex.: "1","2")
  out <- suppressWarnings(as.numeric(x_chr))
  # se maioria virou NA, mapear textos
  if(mean(is.na(out)) > 0.5) {
    out <- dplyr::case_when(
      x_chr %in% c("1","discordo totalmente","totalmente em desacordo") ~ 1,
      x_chr %in% c("2","discordo","discordo parcialmente") ~ 2,
      x_chr %in% c("3","nem concordo nem discordo","neutro","indiferente") ~ 3,
      x_chr %in% c("4","concordo","concordo parcialmente") ~ 4,
      x_chr %in% c("5","concordo totalmente","totalmente de acordo") ~ 5,
      str_detect(x_chr, "^1") ~ 1,  # cobre "1 - Discordo totalmente"
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

# (Opcional) inverter itens negativos: informe os nomes aqui
itens_invertidos <- c() # ex.: c("Likert_2","Likert_5")
if(length(itens_invertidos)) {
  colunas_likert_num[itens_invertidos] <- lapply(colunas_likert_num[itens_invertidos], function(x) 6 - x)
}

# 7) Escore Likert + Alfa
dados_respostas$escore_likert <- rowSums(colunas_likert_num, na.rm = TRUE)

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
    mean(dados_respostas$escore_likert, na.rm = TRUE),
    median(dados_respostas$escore_likert, na.rm = TRUE),
    sd(dados_respostas$escore_likert, na.rm = TRUE),
    IQR(dados_respostas$escore_likert, na.rm = TRUE),
    min(dados_respostas$escore_likert, na.rm = TRUE),
    max(dados_respostas$escore_likert, na.rm = TRUE)
  )
)

# 8) Thurstone: binarizar respostas e aplicar pesos
to_bin_thurstone <- function(x){
  x_chr <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("concordo","concordo totalmente","concordo parcialmente","sim","verdadeiro","true","1") ~ 1,
    x_chr %in% c("discordo","discordo totalmente","discordo parcialmente","nao","não","falso","false","0") ~ 0,
    suppressWarnings(!is.na(as.numeric(x_chr)) & as.numeric(x_chr) > 0) ~ 1, # numérico > 0
    suppressWarnings(!is.na(as.numeric(x_chr)) & as.numeric(x_chr) == 0) ~ 0,
    TRUE ~ NA_real_
  )
}
colunas_thurstone_bin <- as.data.frame(lapply(colunas_thurstone, to_bin_thurstone))
names(colunas_thurstone_bin) <- names(colunas_thurstone)

# Pesos: detectar coluna de "Mediana (peso final)" no CSV de pesos
nome_col_peso <- grep("peso|mediana", names(dados_pesos), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(nome_col_peso))

# converter pesos para numérico (troca vírgula por ponto, se houver)
pesos_thurstone <- dados_pesos[[nome_col_peso]]
if(is.character(pesos_thurstone)) pesos_thurstone <- as.numeric(gsub(",", ".", pesos_thurstone))
pesos_thurstone <- as.numeric(pesos_thurstone)

# manter apenas a quantidade de itens da sua aplicação (5 itens K:O)
pesos_thurstone <- pesos_thurstone[1:ncol(colunas_thurstone_bin)]
stopifnot(length(pesos_thurstone) == ncol(colunas_thurstone_bin))

# escore Thurstone = média dos pesos das afirmativas com concordo (1)
mat_bin <- as.matrix(colunas_thurstone_bin)
soma_pesos <- mat_bin %*% matrix(pesos_thurstone, ncol = 1)
qtd_conc   <- rowSums(colunas_thurstone_bin, na.rm = TRUE)
dados_respostas$escore_thurstone <- ifelse(qtd_conc > 0, soma_pesos[,1] / qtd_conc, NA_real_)

# Estatísticas Thurstone
thurstone_stats <- data.frame(
  Estatistica = c("Média","Mediana","Desvio Padrão","IQR","Min","Max"),
  Valor = c(
    mean(dados_respostas$escore_thurstone, na.rm = TRUE),
    median(dados_respostas$escore_thurstone, na.rm = TRUE),
    sd(dados_respostas$escore_thurstone, na.rm = TRUE),
    IQR(dados_respostas$escore_thurstone, na.rm = TRUE),
    min(dados_respostas$escore_thurstone, na.rm = TRUE),
    max(dados_respostas$escore_thurstone, na.rm = TRUE)
  )
)

cat("\nResumo Likert:\n"); print(likert_stats)
cat("\nResumo Thurstone:\n"); print(thurstone_stats)

# 9) Gráficos
dir.create("plots", showWarnings = FALSE)

g_hist_likert <- ggplot(dados_respostas, aes(x = escore_likert)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribuição de Escores - Likert", x = "Escore total", y = "Frequência") +
  theme_minimal()

g_box_likert <- ggplot(dados_respostas, aes(y = escore_likert)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot - Likert", y = "Escore total") +
  theme_minimal()

g_hist_thurst <- ggplot(dados_respostas, aes(x = escore_thurstone)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black", alpha = 0.8) +
  labs(title = "Distribuição de Escores - Thurstone", x = "Escore", y = "Frequência") +
  theme_minimal()

g_box_thurst <- ggplot(dados_respostas, aes(y = escore_thurstone)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot - Thurstone", y = "Escore") +
  theme_minimal()

# Comparação e correlação
dados_comp <- dados_respostas %>%
  select(escore_likert, escore_thurstone) %>%
  pivot_longer(cols = everything(), names_to = "Escala", values_to = "Escore")

g_box_comp <- ggplot(dados_comp, aes(x = Escala, y = Escore, fill = Escala)) +
  geom_boxplot() +
  scale_fill_manual(values = c("escore_likert" = "skyblue", "escore_thurstone" = "lightgreen")) +
  labs(title = "Comparação de Escores", x = "", y = "Escore") +
  theme_minimal()

cor_escalas <- suppressWarnings(cor(dados_respostas$escore_likert,
                                    dados_respostas$escore_thurstone,
                                    use = "complete.obs", method = "pearson"))
cat("\nCorrelação Likert x Thurstone (pearson):", round(cor_escalas, 3), "\n")

g_scatter <- ggplot(dados_respostas, aes(escore_likert, escore_thurstone)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = paste0("Correlação Likert x Thurstone (r = ", round(cor_escalas, 3), ")"),
       x = "Escore Likert", y = "Escore Thurstone") +
  theme_minimal()

# Salvar gráficos
ggsave("plots/hist_likert.png", g_hist_likert, width = 6, height = 4, dpi = 150)
ggsave("plots/box_likert.png", g_box_likert, width = 4, height = 4, dpi = 150)
ggsave("plots/hist_thurstone.png", g_hist_thurst, width = 6, height = 4, dpi = 150)
ggsave("plots/box_thurstone.png", g_box_thurst, width = 4, height = 4, dpi = 150)
ggsave("plots/box_comparativo.png", g_box_comp, width = 5, height = 4, dpi = 150)
ggsave("plots/scatter_correlacao.png", g_scatter, width = 5, height = 4, dpi = 150)

# 10) Exportar escores finais para uso no R Markdown
saida <- dados_respostas %>%
  mutate(across(everything(), as.character)) %>%
  bind_cols(
    colunas_likert_num %>% mutate(across(everything(), as.character)),
    colunas_thurstone %>% mutate(across(everything(), as.character))
  ) %>%
  select(escore_likert, escore_thurstone, everything())

write_csv(saida, "escores_finais.csv")
cat("\nArquivo 'escores_finais.csv' salvo e gráficos na pasta 'plots/'.\n")