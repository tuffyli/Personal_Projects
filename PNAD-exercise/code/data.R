# ---------------------------------------------------------------------------- #
# PNAD data preparation for the alimony rights exercise
# Last edited by: Tuffy Licciardi Issa
# Date: 2026-05-13
# ---------------------------------------------------------------------------- #

library(dplyr)

project_dir <- normalizePath(
  if (dir.exists("data") && dir.exists("code")) {
    "."
  } else if (dir.exists(file.path("..", "data")) && dir.exists(file.path("..", "code"))) {
    ".."
  } else {
    stop("Run this script from the PNAD-exercise folder or its code subfolder.")
  },
  winslash = "/"
)

project_path <- function(...) file.path(project_dir, ...)

# ---------------------------------------------------------------------------- #
# Data paths
# ---------------------------------------------------------------------------- #

# Optional raw PNAD file. This file is not included in the repository.
# To rebuild the analysis data from raw microdata, either:
# 1. place the raw file at this path, or
# 2. load the raw PNAD data into a data frame named `data` before sourcing this file.
raw_data_path <- project_path("data", "pnad_raw_9295.rds")

# Included/intermediate files used by the public code sample.
filtered_data_path <- project_path("data", "Pnadpnad_filtered_9295.rds")
final_data_path <- project_path("data", "final_filtered_9295.rds")
summary_path <- project_path("data", "summary.rds")

# ---------------------------------------------------------------------------- #
# 1. Variable selection
# ---------------------------------------------------------------------------- #

is_raw_pnad <- function(x) {
  is.data.frame(x) && all(c("ID_DOMICILIO", "v0101", "v4614") %in% names(x))
}

has_raw_data_object <- exists("data", envir = .GlobalEnv, inherits = FALSE) &&
  is_raw_pnad(get("data", envir = .GlobalEnv))

if (has_raw_data_object || file.exists(raw_data_path)) {
  raw_data <- if (has_raw_data_object) {
    get("data", envir = .GlobalEnv)
  } else {
    readRDS(raw_data_path)
  }

  if (!is_raw_pnad(raw_data)) {
    stop(
      "`pnad_raw_9295.rds` does not have the expected raw PNAD columns. ",
      "Expected at least ID_DOMICILIO, v0101, and v4614."
    )
  }

  filtered_data <- raw_data %>%
    group_by(ID_DOMICILIO) %>%
    mutate(
      num_moradores = n(),
      renda_dom_pc = case_when(
        is.na(first(v4614)) | first(v4614) == 999999999999 ~ NA_real_,
        num_moradores > 0 ~ first(v4614) / num_moradores,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(
      v0101, uf, v0102, v0103, v0301, v0302, ID_DOMICILIO,
      v8005, v0401, v0404,
      v0607, v0610, v0602, v0603, v4703,
      v9001, v0713, v7122, v7125, v7127, v7128, v1254,
      v4721, v4614, renda_dom_pc,
      v9906, v9907, v0701,
      v1141, v1142, v1151, v1152,
      v4723, v0501,
      v4729, any_of("treatment"),
      v1001, v1002, v0402
    ) %>%
    rename(
      ano = v0101,
      id_domicilio = ID_DOMICILIO,
      num_controle_dom = v0102,
      num_serie_dom = v0103,
      num_ordem_morador = v0301,
      age = v8005,
      sex = v0302,
      condicao_no_dom = v0401,
      cor = v0404,
      curso_mais_elevado = v0607,
      ultima_serie_concluida = v0610,
      frequenta_escola = v0602,
      tipo_curso_frequenta = v0603,
      anos_estudo = v4703,
      trabalhou_semana_ref = v9001,
      horas_trabalhadas = v0713,
      renda_trab_dinheiro = v7122,
      renda_trab_produto = v7125,
      cod_renda_beneficio = v7127,
      indicador_nao_remunerado = v7128,
      pensao = v1254,
      renda_dom_total = v4721,
      renda_dom_total_v2 = v4614,
      renda_dom_per_capita = renda_dom_pc,
      trabalhou_ultimo_ano = v0701,
      cod_ocupacao = v9906,
      cod_atividade = v9907,
      filhos_homens_dom = v1141,
      filhos_mulheres_dom = v1142,
      filhos_homens_outrolocal = v1151,
      filhos_mulheres_outrolocal = v1152,
      tipo_familia = v4723,
      nasceu_no_municipio = v0501,
      house_status = v0402,
      union_cond = v1002,
      union_status = v1001,
      peso_pessoa = v4729
    )

  saveRDS(filtered_data %>% select(-any_of("treatment")), filtered_data_path)
} else if (file.exists(filtered_data_path)) {
  filtered_data <- readRDS(filtered_data_path)
} else {
  stop(
    "No PNAD input was found. Load the raw PNAD data into an object named ",
    "`data`, place `pnad_raw_9295.rds` in the data folder, or keep ",
    "`Pnadpnad_filtered_9295.rds` in the data folder."
  )
}

# ---------------------------------------------------------------------------- #
# 2. Treatment definition and analysis variables
# ---------------------------------------------------------------------------- #

analysis_data <- filtered_data %>%
  select(-any_of("treatment")) %>%
  mutate(
    treatment = case_when(
      sex == 4 &
        between(age, 15, 24) &
        union_status == 1 &
        union_cond %in% c(6, 8) &
        house_status %in% c(1, 2) ~ 1L,
      sex == 4 &
        between(age, 15, 24) &
        union_status == 1 &
        union_cond %in% c(2, 4) &
        house_status %in% c(1, 2) ~ 0L,
      TRUE ~ NA_integer_
    ),
    cor = case_when(
      cor %in% c(2, 6) ~ 1,
      cor == 9 ~ NA_real_,
      TRUE ~ 0
    ),
    grupo_cbo = case_when(
      !is.na(cod_ocupacao) & cod_ocupacao < 900 ~ cod_ocupacao %/% 100,
      TRUE ~ NA_real_
    ),
    ano = ano + 1900,
    trabalhou_ultimo_ano = if_else(trabalhou_ultimo_ano == 1, 1, 0),
    fem = if_else(sex == 4, 1, 0),
    pensao_dummy = if_else(!is.na(pensao), 1, 0),
    dummy_filhos_homens_dom = if_else(
      !filhos_homens_dom %in% c(-1, 99),
      filhos_homens_dom,
      0
    ),
    dummy_filhos_mulheres_dom = if_else(
      !filhos_mulheres_dom %in% c(-1, 99),
      filhos_mulheres_dom,
      0
    ),
    dummy_filhos_homens_outrolocal = if_else(
      !filhos_homens_outrolocal %in% c(-1, 99),
      filhos_homens_outrolocal,
      0
    ),
    dummy_filhos_mulheres_outrolocal = if_else(
      !filhos_mulheres_outrolocal %in% c(-1, 99),
      filhos_mulheres_outrolocal,
      0
    ),
    total_filhos = dummy_filhos_homens_dom + dummy_filhos_mulheres_dom +
      dummy_filhos_homens_outrolocal + dummy_filhos_mulheres_outrolocal
  ) %>%
  filter(!is.na(treatment))

# ---------------------------------------------------------------------------- #
# 3. Weighted descriptive statistics
# ---------------------------------------------------------------------------- #

weighted_stats <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  x <- as.numeric(x[valid])
  w <- as.numeric(w[valid])

  if (length(x) == 0) {
    return(c(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_))
  }

  w_mean <- weighted.mean(x, w)
  w_sd <- sqrt(sum(w * (x - w_mean)^2) / sum(w))

  c(
    mean = w_mean,
    sd = w_sd,
    min = min(x),
    max = max(x)
  )
}

summary_vars <- c(
  "treatment", "fem", "cor", "curso_mais_elevado", "age", "anos_estudo",
  "ultima_serie_concluida", "tipo_curso_frequenta", "renda_dom_per_capita",
  "pensao_dummy", "grupo_cbo", "dummy_filhos_homens_dom",
  "dummy_filhos_mulheres_dom", "total_filhos"
)

summary_labels <- c(
  "Treatment", "Female = 1", "Race (White or Asian = 1)",
  "Highest education", "Age", "Years of education",
  "Last grade completed", "Course enrollment",
  "Household per-capita income", "Pension/alimony receipt = 1",
  "CBO group", "Male children in household",
  "Female children in household", "Total children"
)

summary_table <- do.call(rbind, lapply(summary_vars, function(variable) {
  weighted_stats(analysis_data[[variable]], analysis_data$peso_pessoa)
})) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ round(.x, 2))) %>%
  setNames(c("Mean", "SD", "Min", "Max"))

rownames(summary_table) <- summary_labels

print(summary_table)

saveRDS(summary_table, summary_path)
saveRDS(analysis_data, final_data_path)
