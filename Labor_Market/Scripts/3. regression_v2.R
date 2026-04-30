# ---------------------------------------------------------------------------- #
# Overall Working Paper Code
# Main regression and results
# Last edited by: Tuffy Licciardi Issa
# Date: 25/04/2026
# Fully commented companion version with light optimizations for readability.
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(dplyr)
library(fixest)
library(ggplot2)
library(did)
library(data.table)
library(zoo)
library(stargazer)
library(gridExtra)
library(knitr)
library(kableExtra)
library(grid)
library(didimputation)
library(DIDmultiplegt)
library(tidyr)

# Centralizing the base directory makes path changes easier later.
root_dir <- "C:/Users/tuffy/Documents/IC"
setwd(root_dir)

# ---------------------------------------------------------------------------- #
# 1. DATA
# ---------------------------------------------------------------------------- #

# Starting a timer so the full execution time can be reported at the end.
start_time <- Sys.time()

# Main estimation base.
data <- read.csv(file.path(root_dir, "Bases/base_atual_dum_v3.csv"))

# ---------------------------------------------------------------------------- #
# 1.1 Result Tables ----
# ---------------------------------------------------------------------------- #

# Row labels used in the final LaTeX tables.
rows_main <- c("ATT", " ", "Pre-Avg", " ", "Observations")

# Small helper to create result tables with the same structure.
make_result_table <- function(rows_vec) {
  data.frame(
    names = rows_vec,
    att_nc = rep(NA, times = length(rows_vec)),  # No controls
    att_fc = rep(NA, times = length(rows_vec)),  # Full controls
    att_bc = rep(NA, times = length(rows_vec)),  # Blue-collar
    att_wc = rep(NA, times = length(rows_vec))   # White-collar
  )
}

# These tables receive the final point estimates and standard errors.
result_rais <- make_result_table(rows_main)
result_cbo  <- make_result_table(rows_main)
result_cnae <- make_result_table(rows_main)
result_sal  <- make_result_table(rows_main)

rm(rows_main)

# ---------------------------------------------------------------------------- #
# 1.2 Helper Functions ----
# ---------------------------------------------------------------------------- #

# Returns significance stars based on a normal-approximation p-value.
sig_stars <- function(est, se) {
  p_value <- 2 * pnorm(abs(est / se), lower.tail = FALSE)

  if (p_value < 0.01) {
    "***"
  } else if (p_value < 0.05) {
    "**"
  } else if (p_value < 0.10) {
    "*"
  } else {
    ""
  }
}

# Formats one ATT estimate with stars and one SE in brackets.
format_att <- function(att, se, with_stars = FALSE) {
  if (with_stars) {
    paste0(round(att, 4), sig_stars(att, se))
  } else {
    round(att, 4)
  }
}

# This filter is repeated throughout the salary sections. Wrapping it once
# avoids copy-paste and keeps the underlying logic unchanged.
salary_sample_filter <- function(df) {
  df %>%
    group_by(code_id) %>%
    filter(
      all(cnae_group[ano < year_first_treated] ==
            cnae_pre_treat[ano < year_first_treated]),
      all_in_rais == 1
    ) %>%
    ungroup()
}

# Computes the average pre-treatment dynamic ATT and its equal-weighted SE.
pre_avg_stats <- function(att_gt_obj) {
  pre_dyn <- aggte(att_gt_obj, type = "dynamic", na.rm = TRUE)

  # Selects event times before treatment.
  pre_egt <- pre_dyn$egt < 0

  # Mean placebo effect across pre-treatment periods.
  pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

  # Influence-function-based covariance matrix used for the SE.
  es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n

  # The original code uses the first 8 pre-treatment coefficients.
  V8 <- V[1:8, 1:8]
  w <- rep(1 / 8, 8)
  var_equal <- as.numeric(crossprod(w, V8 %*% w))
  se_equal <- sqrt(var_equal)

  list(pre_av = pre_av, se_equal = se_equal, pre_dyn = pre_dyn)
}

# Builds the final LaTeX table in the paper-ready format used in the draft.
make_final_tex_table <- function(df, title, file) {
  table_out <- data.frame(
    " " = c("ATT", "", "Pre-Avg", "", "Observations", "Controls"),
    "Total\n(1)" = c(df$att_nc[1], df$att_nc[2], df$att_nc[3], df$att_nc[4], df$att_nc[5], "No"),
    "Total\n(2)" = c(df$att_fc[1], df$att_fc[2], df$att_fc[3], df$att_fc[4], df$att_fc[5], "Yes"),
    "Blue\n(3)" = c(df$att_bc[1], df$att_bc[2], df$att_bc[3], df$att_bc[4], df$att_bc[5], "Yes"),
    "White\n(4)" = c(df$att_wc[1], df$att_wc[2], df$att_wc[3], df$att_wc[4], df$att_wc[5], "Yes"),
    check.names = FALSE
  )

  latex_table <- kbl(
    table_out,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c"),
    escape = FALSE,
    caption = title,
    linesep = ""
  ) %>%
    add_header_above(c(" " = 3, "Occupation Heterogeneity" = 2)) %>%
    kable_styling(
      latex_options = c("hold_position"),
      full_width = FALSE,
      position = "center"
    ) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(c(1, 3, 5, 6), bold = TRUE) %>%
    footnote(
      general = "The Callaway and Sant'Anna (2021b) estimator is implemented through the did R package. The Pre-Avg is calculated by taking the simple mean of values before treatment. Significance at the 10\\% level is represented by *, at the 5\\% level by **, and at the 1\\% level by ***.",
      threeparttable = TRUE,
      escape = FALSE
    )

  writeLines(as.character(latex_table), file)
}

# ---------------------------------------------------------------------------- #
# 2. Main Graphs ----
## 2.1 Function ----
# ---------------------------------------------------------------------------- #

plot <- function(df,
                 plot_title,
                 var_y,
                 controles) {

  # Starts the timer for this specific estimation routine.
  ini <- Sys.time()

  # Converts the outcome name into a character string for use in formulas.
  var_y <- as.character(substitute(var_y))
  message("Calculando para: ", var_y, " :)")

  # -------------------------------------------------------------------------- #
  # Model formulas
  # -------------------------------------------------------------------------- #

  # Sun & Abraham event-study specification with controls and fixed effects.
  sunab_formula <- as.formula(
    paste(
      var_y,
      "~ sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | code_id + ano_sexo + ano_branco + ano_ensino"
    )
  )

  # Control formula used by Callaway & Sant'Anna.
  calsan_formula <- as.formula(
    "~ ano_sexo + ano_branco + ano_ensino + code_id"
  )

  # -------------------------------------------------------------------------- #
  # Estimations
  # -------------------------------------------------------------------------- #

  # Sun & Abraham estimation.
  est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)

  # Callaway & Sant'Anna group-time ATT estimation.
  calsan_did <- did::att_gt(
    yname = var_y,
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = calsan_formula,
    data = df,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )

  # Dynamic aggregation for event-study plotting.
  est_calsan <- aggte(MP = calsan_did, type = "dynamic", na.rm = TRUE)
  print(est_calsan)

  # -------------------------------------------------------------------------- #
  # Plot extraction
  # -------------------------------------------------------------------------- #

  # `iplot()` is used only to extract the plotted Sun & Abraham coefficients.
  plot_sunab <- iplot(
    est_sunab,
    ref.line = -1,
    xlab = "Time to treatment",
    main = "Cal_San ES: IGNORAR"
  )

  # `ggdid()` is used only to extract the plotted Callaway & Sant'Anna data.
  plot_calsan <- ggdid(est_calsan) +
    ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
    theme_minimal()

  # Extracts the data behind the Callaway & Sant'Anna ggplot object.
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan <- as.data.frame(data_calsan)

  # Extracts the data behind the Sun & Abraham plot object.
  data_sunab <- plot_sunab[[1]]
  data_sunab <- data_sunab %>%
    mutate(colour = ifelse(id == 1, "#f7200a", NA)) %>%
    rename(
      ymin = ci_low,
      ymax = ci_high,
      group = id
    ) %>%
    select(
      colour, x, y, ymin, ymax, group,
      -estimate, -estimate_names, -estimate_names_raw, -is_ref
    )

  # Standardizes the Callaway & Sant'Anna output so it matches the Sun & Abraham
  # object structure and can be row-bound below.
  data_calsan <- data_calsan %>%
    mutate(
      colour = "#145ede",
      group = 2,
      y = ifelse(x == -1, 0, y),
      ymin = ifelse(x == -1, 0, ymin),
      ymax = ifelse(x == -1, 0, ymax)
    ) %>%
    select(-PANEL, -shape, -size, -fill, -alpha, -stroke)

  # Combines the two event-study series into one graph-ready data frame.
  df_completo <- rbind(data_sunab, data_calsan)

  # Prints full estimation summaries to the console.
  print(summary(est_sunab))
  print(summary(est_calsan))

  # Shifts one series slightly on the x-axis so points and error bars do not
  # fully overlap in the final graph.
  df_completo$x <- case_when(
    df_completo$group == 1 ~ df_completo$x,
    df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
    TRUE ~ NA_real_
  )

  # -------------------------------------------------------------------------- #
  # Timing
  # -------------------------------------------------------------------------- #

  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)

  print("---------------------------------------------")
  print(paste0("Total time elapsed: ", mins, " mins e ", secs, " s"))
  print("---------------------------------------------")

  rm(delta, ini, fim, mins, secs)

  # Returns the combined coefficient data frame.
  return(df_completo)
}

# ---------------------------------------------------------------------------- #
## 2.2 RAIS ----
# ---------------------------------------------------------------------------- #

# Estimates the main event-study coefficients for the RAIS outcome.
estimacoes_rais <- plot(
  data,
  plot_title = "",
  var_y = "rais_"
)

# ---------------------------------------------------------------------------- #
## Removing the SUNAB estimator ----
# ---------------------------------------------------------------------------- #

# Keeps only Callaway & Sant'Anna and removes the small x-axis offset used for
# the two-estimator comparison graph.
est_rais2 <- estimacoes_rais %>%
  filter(group == 2) %>%
  mutate(x = x - 0.2)

# Final cleaner graph for the paper.
p <- ggplot(est_rais2, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = "black", labels = "Callaway & Sant'anna") +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    legend.position = "none",
    axis.text.y = element_text(size = 18)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4"),
    minor_breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4)
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.155),
    breaks = c(-0.90, -0.75, -0.60, -0.45, -0.30, -0.15, 0, 0.15),
    labels = c("-0.90", "-0.75", "-0.60", "-0.45", "-0.30", "-0.15", "0", "0.15")
  )

p

ggsave(file.path(root_dir, "Graphs/united/plot_rais2_v3.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(file.path(root_dir, "Graphs/united/plot_rais2_v3.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
## 2.3 CBO ----
# ---------------------------------------------------------------------------- #

# Main event-study estimation for the occupational classification outcome.
estimacoes_cbo <- plot(
  data,
  plot_title = "",
  var_y = "dummy_cbo"
)

p <- ggplot(estimacoes_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("Callaway & Sant'anna", "Sun & Abraham")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    limits = c(-9.2, 5.2),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4", "+5")
  ) +
  scale_y_continuous(
    limits = c(-0.155, 0.95),
    breaks = c(-0.15, 0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90),
    labels = c("-0.15", "0", "0.15", "0.30", "0.45", "0.60", "0.75", "0.90")
  )

ggsave(file.path(root_dir, "Graphs/plot_cbo.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

rm(p)

# ---------------------------------------------------------------------------- #
## 2.4 CNAE ----
# ---------------------------------------------------------------------------- #

# Main event-study estimation for industry classification mobility.
estimacoes_cnae <- plot(
  data,
  plot_title = "",
  var_y = "dummy_cnae"
)

p <- ggplot(estimacoes_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("Callaway & Sant'anna", "Sun & Abraham")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    limits = c(-9.2, 5.2),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4", "+5")
  ) +
  scale_y_continuous(
    limits = c(-0.155, 1.10),
    breaks = c(-0.15, 0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 1.05),
    labels = c("-0.15", "0", "0.15", "0.30", "0.45", "0.60", "0.75", "0.90", "1.05")
  )

ggsave(file.path(root_dir, "Graphs/plot_cnae.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

rm(p)
gc()

# ---------------------------------------------------------------------------- #
## 2.5 Salary -----
# ---------------------------------------------------------------------------- #

# Salary uses the restricted sample that preserves CNAE consistency before
# treatment and keeps only observations fully present in RAIS.
estimacoes_sal <- plot(
  salary_sample_filter(data),
  plot_title = "",
  var_y = "remuneracao_media_sm_"
)

p <- ggplot(
  estimacoes_sal %>%
    filter(group == 2) %>%
    mutate(x = x - 0.2),
  aes(x = x, y = y, color = colour, group = group)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = "black", labels = "Callaway & Sant'anna") +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    legend.position = "none",
    axis.text.y = element_text(size = 18)
  ) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4"),
    minor_breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4)
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.45),
    breaks = seq(-0.6, 0.45, by = 0.15),
    labels = sprintf("%.2f", seq(-0.6, 0.45, by = 0.15))
  )

p

ggsave(file.path(root_dir, "Graphs/final/plot_sal.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ---------------------------------------------------------------------------- #
# 3. White vs. Blue ----
# ---------------------------------------------------------------------------- #
## 3.1 Data Frames ----
# ---------------------------------------------------------------------------- #

# Splits the main data by worker type.
blue_data <- data %>%
  filter(white_dummy == 0)

white_data <- data %>%
  filter(white_dummy == 1)

# ---------------------------------------------------------------------------- #
## 3.2 Blue Collar ----
### 3.2.1 RAIS ----
# ---------------------------------------------------------------------------- #

estimacoes_brais <- plot(
  blue_data,
  plot_title = "",
  var_y = "rais_"
)

# ---------------------------------------------------------------------------- #
### 3.2.2 CBO ----
# ---------------------------------------------------------------------------- #

estimacoes_bcbo <- plot(
  blue_data,
  plot_title = "",
  var_y = "dummy_cbo"
)

# ---------------------------------------------------------------------------- #
### 3.2.3 CNAE ----
# ---------------------------------------------------------------------------- #

estimacoes_bcnae <- plot(
  blue_data,
  plot_title = "",
  var_y = "dummy_cnae"
)

# ---------------------------------------------------------------------------- #
### 3.2.4 Salary ----
# ---------------------------------------------------------------------------- #

estimacoes_bsal <- plot(
  salary_sample_filter(blue_data),
  plot_title = "",
  var_y = "remuneracao_media_sm_"
)

# ---------------------------------------------------------------------------- #
## 3.3 White Collar ----
# ---------------------------------------------------------------------------- #
### 3.3.1 RAIS ----
# ---------------------------------------------------------------------------- #

estimacoes_wrais <- plot(
  white_data,
  plot_title = "",
  var_y = "rais_"
)

# ---------------------------------------------------------------------------- #
### 3.3.2 CBO ----
# ---------------------------------------------------------------------------- #

estimacoes_wcbo <- plot(
  white_data,
  plot_title = "",
  var_y = "dummy_cbo"
)

# ---------------------------------------------------------------------------- #
### 3.3.3 CNAE ----
# ---------------------------------------------------------------------------- #

estimacoes_wcnae <- plot(
  white_data,
  plot_title = "",
  var_y = "dummy_cnae"
)

# ---------------------------------------------------------------------------- #
### 3.3.4 Salary ----
# ---------------------------------------------------------------------------- #

estimacoes_wsal <- plot(
  salary_sample_filter(white_data),
  plot_title = "",
  var_y = "remuneracao_media_sm_"
)

# ---------------------------------------------------------------------------- #
# 4. New Spec (WxB)----
# ---------------------------------------------------------------------------- #
## 4.1 Data ----
# ---------------------------------------------------------------------------- #

# Helper for binding white- and blue-collar estimation outputs into one graph
# object while keeping only the Callaway & Sant'Anna series.
bind_collar_results <- function(blue_est, white_est) {
  blue_est$collar <- 0
  white_est$collar <- 1

  rbind(blue_est, white_est) %>%
    filter(group == 2) %>%
    mutate(
      group = ifelse(collar == 1, 1, group),
      x = ifelse(collar == 1, x - 0.2, x),
      colour = ifelse(group == 2, "#f7200a", colour)
    ) %>%
    select(-collar)
}

# ---------------------------------------------------------------------------- #
### 4.1.1 RAIS ----
# ---------------------------------------------------------------------------- #

both_rais <- bind_collar_results(estimacoes_brais, estimacoes_wrais)

# ---------------------------------------------------------------------------- #
### 4.1.2 CBO ----
# ---------------------------------------------------------------------------- #

both_cbo <- bind_collar_results(estimacoes_bcbo, estimacoes_wcbo)

# ---------------------------------------------------------------------------- #
### 4.1.3 CNAE ----
# ---------------------------------------------------------------------------- #

both_cnae <- bind_collar_results(estimacoes_bcnae, estimacoes_wcnae)

# ---------------------------------------------------------------------------- #
### 4.1.4 Salary ----
# ---------------------------------------------------------------------------- #

both_sal <- bind_collar_results(estimacoes_bsal, estimacoes_wsal)

# ---------------------------------------------------------------------------- #
### 4.1.5 Saving ----
# ---------------------------------------------------------------------------- #

# Uncomment these lines if you want to save the graph-ready coefficient objects.
# saveRDS(estimacoes_rais, file.path(root_dir, "Bases/results_est/rais_total.RDS"))
# saveRDS(both_rais, file.path(root_dir, "Bases/results_est/rais_both_wc.RDS"))
# saveRDS(both_cbo, file.path(root_dir, "Bases/results_est/cbo_both_wc.RDS"))
# saveRDS(both_cnae, file.path(root_dir, "Bases/results_est/cnae_both_wc.RDS"))
# saveRDS(both_sal, file.path(root_dir, "Bases/results_est/sal_both_wc.RDS"))

# ---------------------------------------------------------------------------- #
## 4.2 Estimation ----
### 4.2.1 RAIS ----
# ---------------------------------------------------------------------------- #

p <- ggplot(both_rais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("White-collar", "Blue-collar")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(0.05, 0.05),
    legend.justification = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4")
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.155),
    breaks = c(-0.90, -0.75, -0.60, -0.45, -0.30, -0.15, 0, 0.15),
    labels = c("-0.90", "-0.75", "-0.60", "-0.45", "-0.30", "-0.15", "0", "0.15")
  )

ggsave(file.path(root_dir, "Graphs/united/plot_rais_wb_col.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(file.path(root_dir, "Graphs/united/plot_rais_wb_col.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
### 4.2.2 CBO ----
# ---------------------------------------------------------------------------- #

p <- ggplot(both_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("White-collar", "Blue-collar")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),
    legend.justification = c(1, 1)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4")
  ) +
  scale_y_continuous(
    limits = c(-0.155, 1.10),
    breaks = c(-0.15, 0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 1.05),
    labels = c("-0.15", "0", "0.15", "0.30", "0.45", "0.60", "0.75", "0.90", "1.05")
  )

p

ggsave(file.path(root_dir, "Graphs/united/plot_cbo_wb_col.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(file.path(root_dir, "Graphs/united/plot_cbo_wb_col.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
### 4.2.3 CNAE ----
# ---------------------------------------------------------------------------- #

p <- ggplot(both_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("White-collar", "Blue-collar")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),
    legend.justification = c(1, 1)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4")
  ) +
  scale_y_continuous(
    limits = c(-0.155, 1.10),
    breaks = c(-0.15, 0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 1.05),
    labels = c("-0.15", "0", "0.15", "0.30", "0.45", "0.60", "0.75", "0.90", "1.05")
  )

p

ggsave(file.path(root_dir, "Graphs/united/plot_cnae_wb_col.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(file.path(root_dir, "Graphs/united/plot_cnae_wb_col.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
### 4.2.4 Salary ----
# ---------------------------------------------------------------------------- #

p <- ggplot(both_sal, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values = c("black", "red"), labels = c("White-collar", "Blue-collar")) +
  labs(x = "Years to treatment", y = "", colour = "") +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 0.25),
    legend.justification = c(1, 1)
  ) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4"),
    minor_breaks = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4)
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.45),
    breaks = seq(-0.6, 0.45, by = 0.15),
    labels = sprintf("%.2f", seq(-0.6, 0.45, by = 0.15))
  )

p

ggsave(file.path(root_dir, "Graphs/final/plot_sal_wb_col.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(file.path(root_dir, "Graphs/final/pdf/plot_sal_wb_col.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
# 5. More analysis ----
# ---------------------------------------------------------------------------- #

# Descriptive statistics for the whole sample.
stargazer(
  data,
  summary = TRUE,
  summary.stat = c("n", "mean", "sd", "median", "min", "max"),
  type = "latex",
  label = "tab:desc_all",
  file = file.path(root_dir, "Tables/total_desc.tex")
)

# Descriptive statistics for blue-collar workers.
stargazer(
  blue_data,
  summary = TRUE,
  summary.stat = c("n", "mean", "sd", "median", "min", "max"),
  type = "latex",
  label = "tab:desc_blue",
  file = file.path(root_dir, "Tables/blue_desc.tex")
)

# Descriptive statistics for white-collar workers.
stargazer(
  white_data,
  summary = TRUE,
  summary.stat = c("n", "mean", "sd", "median", "min", "max"),
  type = "latex",
  label = "tab:desc_white",
  file = file.path(root_dir, "Tables/white_desc.tex")
)

# Simple difference-in-means test for salary by collar.
t.test(remuneracao_media_sm_ ~ white_dummy, data = data)

# ---------------------------------------------------------------------------- #
# 6. ATT Values ----
# ---------------------------------------------------------------------------- #
## 6.1 RAIS ----
### 6.1.1 No Controls ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

# Stores the overall ATT and SE in the results table.
result_rais$att_nc[1] <- est_calsan_sc1$overall.att
result_rais$att_nc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.1.2 With Controls ----
# ---------------------------------------------------------------------------- #

w_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = w_control, type = "dynamic", na.rm = TRUE)

result_rais$att_fc[1] <- est_calsan_sc1$overall.att
result_rais$att_fc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

rm(w_control)

# ---------------------------------------------------------------------------- #
## 6.2 CBO ----
# ---------------------------------------------------------------------------- #
### 6.2.1 No Controls ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

# ---------------------------------------------------------------------------- #
### 6.2.2 Table Both ----
# ---------------------------------------------------------------------------- #

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cbo$att_nc[1] <- est_calsan_sc1$overall.att
result_cbo$att_nc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 6.3 CNAE ----
### 6.3.1 No Controls ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

# ---------------------------------------------------------------------------- #
### 6.3.2 Table Both ----
# ---------------------------------------------------------------------------- #

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cnae$att_nc[1] <- est_calsan_sc1$overall.att
result_cnae$att_nc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 6.4 Salary ----
# ---------------------------------------------------------------------------- #
### 6.4.1 No Controls ----
# ---------------------------------------------------------------------------- #

salary_data <- salary_sample_filter(data)

no_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

# ---------------------------------------------------------------------------- #
#### 6.4.1.1 Table No Controls ----
# ---------------------------------------------------------------------------- #

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

att <- est_calsan_sc1$overall.att
se <- est_calsan_sc1$overall.se

result_sal$att_nc[1] <- format_att(att, se, with_stars = TRUE)
result_sal$att_nc[2] <- paste0("[", round(se, digits = 4), "]")
result_sal$att_nc[5] <- nrow(salary_data)

# ---------------------------------------------------------------------------- #
### 6.4.2 With Controls ----
# ---------------------------------------------------------------------------- #

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = salary_data,
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = w_control, type = "dynamic", na.rm = TRUE)
att <- est_calsan_sc1$overall.att
se <- est_calsan_sc1$overall.se

result_sal$att_fc[1] <- format_att(att, se, with_stars = TRUE)
result_sal$att_fc[2] <- paste0("[", round(se, digits = 4), "]")
result_sal$att_fc[5] <- nrow(salary_data)

rm(w_control)

# ---------------------------------------------------------------------------- #
## 6.5 Blue Collar ----
# ---------------------------------------------------------------------------- #
### 6.5.1 RAIS BC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_rais$att_bc[1] <- est_calsan_sc1$overall.att
result_rais$att_bc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.5.2 CBO BC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cbo$att_bc[1] <- est_calsan_sc1$overall.att
result_cbo$att_bc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.5.3 CNAE BC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cnae$att_bc[1] <- est_calsan_sc1$overall.att
result_cnae$att_bc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.5.4 Salary BC ----
# ---------------------------------------------------------------------------- #

blue_salary_data <- salary_sample_filter(blue_data)

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_salary_data,
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = w_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = w_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

att <- est_calsan_sc1$overall.att
se <- est_calsan_sc1$overall.se

result_sal$att_bc[1] <- format_att(att, se, with_stars = TRUE)
result_sal$att_bc[2] <- paste0("[", round(se, digits = 4), "]")
result_sal$att_bc[5] <- nrow(blue_salary_data)

rm(att, se)

# ---------------------------------------------------------------------------- #
## 6.6 White Collar ----
# ---------------------------------------------------------------------------- #
### 6.6.1 RAIS WC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_rais$att_wc[1] <- est_calsan_sc1$overall.att
result_rais$att_wc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.6.2 CBO WC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cbo$att_wc[1] <- est_calsan_sc1$overall.att
result_cbo$att_wc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.6.3 CNAE WC ----
# ---------------------------------------------------------------------------- #

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = no_control, type = "simple", na.rm = TRUE)
print(est_calsan_sc1)
print(est_calsan_scs)

result_cnae$att_wc[1] <- est_calsan_sc1$overall.att
result_cnae$att_wc[2] <- paste0("[", round(est_calsan_sc1$overall.se, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 6.6.4 Salary WC ----
# ---------------------------------------------------------------------------- #

white_salary_data <- salary_sample_filter(white_data)

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte(MP = w_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte(MP = w_control, type = "simple", na.rm = TRUE)

att <- est_calsan_sc1$overall.att
se <- est_calsan_sc1$overall.se

result_sal$att_wc[1] <- format_att(att, se, with_stars = TRUE)
result_sal$att_wc[2] <- paste0("[", round(se, digits = 4), "]")
result_sal$att_wc[5] <- nrow(white_salary_data)

rm(att, se)

# ---------------------------------------------------------------------------- #
# 7. Pre-Avg ----
# ---------------------------------------------------------------------------- #
## 7.1 RAIS ----
### 7.1.1 With Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_rais$att_fc[3] <- pre_stats$pre_av
result_rais$att_fc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.1.2 No Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_rais$att_nc[3] <- pre_stats$pre_av
result_rais$att_nc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 7.2 CBO ----
### 7.2.1 With Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cbo$att_fc[3] <- pre_stats$pre_av
result_cbo$att_fc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.2.2 No Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cbo$att_nc[3] <- pre_stats$pre_av
result_cbo$att_nc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 7.3 CNAE ----
### 7.3.1 With Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cnae$att_fc[3] <- pre_stats$pre_av
result_cnae$att_fc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.3.2 No Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cnae$att_nc[3] <- pre_stats$pre_av
result_cnae$att_nc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 7.4 Salary ----
### 7.4.1 With Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)

result_sal$att_fc[3] <- format_att(pre_stats$pre_av, pre_stats$se_equal, with_stars = TRUE)
result_sal$att_fc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.4.2 No Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)

result_sal$att_nc[3] <- format_att(pre_stats$pre_av, pre_stats$se_equal, with_stars = TRUE)
result_sal$att_nc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 7.4 Blue Collar ----
### 7.4.1 RAIS ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_rais$att_bc[3] <- pre_stats$pre_av
result_rais$att_bc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.4.2 CBO ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cbo$att_bc[3] <- pre_stats$pre_av
result_cbo$att_bc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.4.3 CNAE ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cnae$att_bc[3] <- pre_stats$pre_av
result_cnae$att_bc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.4.4 Salary - Controles ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)

result_sal$att_bc[3] <- format_att(pre_stats$pre_av, pre_stats$se_equal, with_stars = TRUE)
result_sal$att_bc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
## 7.5 White Collar ----
# ---------------------------------------------------------------------------- #
### 7.5.1 RAIS ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_rais$att_wc[3] <- pre_stats$pre_av
result_rais$att_wc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.5.2 CBO ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cbo$att_wc[3] <- pre_stats$pre_av
result_cbo$att_wc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.5.3 CNAE ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)
print(pre_stats$pre_av)
print(pre_stats$se_equal)

result_cnae$att_wc[3] <- pre_stats$pre_av
result_cnae$att_wc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
### 7.5.4 Salary - Controles ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_salary_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_stats <- pre_avg_stats(pre_avg)

result_sal$att_wc[3] <- format_att(pre_stats$pre_av, pre_stats$se_equal, with_stars = TRUE)
result_sal$att_wc[4] <- paste0("[", round(pre_stats$se_equal, digits = 4), "]")

# ---------------------------------------------------------------------------- #
# 7.6 Observation Rows ----
# ---------------------------------------------------------------------------- #

# Fills the observation row for the non-salary tables so all final LaTeX tables
# share the same structure.
result_rais$att_nc[5] <- nrow(data)
result_rais$att_fc[5] <- nrow(data)
result_rais$att_bc[5] <- nrow(blue_data)
result_rais$att_wc[5] <- nrow(white_data)

result_cbo$att_nc[5] <- nrow(data)
result_cbo$att_fc[5] <- nrow(data)
result_cbo$att_bc[5] <- nrow(blue_data)
result_cbo$att_wc[5] <- nrow(white_data)

result_cnae$att_nc[5] <- nrow(data)
result_cnae$att_fc[5] <- nrow(data)
result_cnae$att_bc[5] <- nrow(blue_data)
result_cnae$att_wc[5] <- nrow(white_data)

# ---------------------------------------------------------------------------- #
# 8. Final Tables ----
# ---------------------------------------------------------------------------- #

# Exports the final tables in the paper-ready format used in the draft.
make_final_tex_table(
  result_rais,
  "Estimated ATT on RAIS",
  file.path(root_dir, "Tables/rais_main.tex")
)

make_final_tex_table(
  result_cbo,
  "Estimated ATT on CBO",
  file.path(root_dir, "Tables/cbo_main.tex")
)

make_final_tex_table(
  result_cnae,
  "Estimated ATT on CNAE",
  file.path(root_dir, "Tables/cnae_main.tex")
)

make_final_tex_table(
  result_sal,
  "Estimated ATT on Salary",
  file.path(root_dir, "Tables/sal_main.tex")
)

# ---------------------------------------------------------------------------- #
# Total elapsed time ----
# ---------------------------------------------------------------------------- #

# Reports the total execution time for the whole script.
final_time <- Sys.time()

delta <- difftime(final_time, start_time, units = "secs")
mins <- floor(as.numeric(delta) / 60)
secs <- round(as.numeric(delta) %% 60)
hours <- floor(as.numeric(mins) / 60)

message("---------------------------------------------")
message("Total time elapsed: ", hours, " hours, ", mins, " mins and ", secs, " s")
message("---------------------------------------------")

# Clears the workspace at the end of the run.
rm(list = ls())
