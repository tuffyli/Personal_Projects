# ---------------------------------------------------------------------------- #
# Combined DrDiD Estimators
# Last edited by: Tuffy Licciardi Issa
# Date: 20/10/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #
library(dplyr)
library(ggplot2)
library(stargazer)
library(gridExtra)
library(knitr)
library(grid)
library(fixest) #Sun & Abraham
library(did) #Callaway & Sant'anna
library(didimputation) #Borusyak et al.
library(DIDmultiplegt) #De Chaisemartin & d'Haultfoeuille


# ---------------------------------------------------------------------------- #
# Paths ----
# ---------------------------------------------------------------------------- #

#'Please insert yout desired data input and output paths.

data_input_path <- "C:/Users/tuffy/Documents/" #Please insert here your data input path

data_output_path <- "C:/Users/tuffy/Documents/"

  
data <- read.csv(data_input_path)


# ------------------------- #
# CalSan & SunAb ----
#-------------------------- #
#' Here I estimate the results for the main estimators I employed, being the ones
#' developed by Callaway and Sant'anna, and Sun and Abraham.

calsun_plot <- function(df,plot_title,var_y,controles) {


  ini <- Sys.time()

  print(paste0("Calculando para:", var_y," :)"))

  var_y <- as.character(substitute(var_y))

  # Equation with controls
  sunab_formula <- as.formula(
    paste( 
      
      # ------------- #
      # Please note that the controls variables must be changed for you own controls
      # ------------- #
      
      var_y, "~  sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | controls"
    )
  )

  calsan_formula <- as.formula(
    "~ controls"
  )
  
  # ---------------------------------- #
  # 1. Estimation ----
  # ---------------------------------- #
  # ---------------------------------- #
  ## 1.1 Sun & Abraham ----
  # ---------------------------------- #
  est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)
  
  # ---------------------------------- #
  ## 1.2 Callaway & Sant'anna ----
  # ---------------------------------- #
  
  
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
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  print(est_calsan)

  # ---------------------------------------------------- #
  #2. Extracting the final graph from the estimators ----
  # ---------------------------------------------------- #
  plot_sunab <- iplot(est_sunab, ref.line = -1,
                      xlab = 'Time to treatment',
                      main = 'Cal_San ES: IGNORAR')

  plot_calsan <- ggdid(est_calsan) +
    ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
    labs("Time to treat") +
    theme_minimal()
  
  
  # --------------------------------------------------- #
  #3. Extracting the coeficients ----
  # --------------------------------------------------- #
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan <- as.data.frame(data_calsan)

  data_sunab <- plot_sunab[[1]]
  data_sunab <- data_sunab %>%
    mutate(colour = ifelse(id == 1, '#f7200a')) %>%
    rename(
      ymin = ci_low,
      ymax = ci_high,
      group = id ) %>%
    select(colour, x, y, ymin, ymax, group,-estimate, -estimate_names, -estimate_names_raw,-is_ref)

  data_calsan <- data_calsan %>%
    mutate(colour = '#145ede',
           group = 2,
           y = ifelse(x == -1, 0, y),
           ymin = ifelse(x == -1, 0, ymin),
           ymax = ifelse(x == -1, 0, ymax)) %>%
    select(-PANEL, -shape, -size, -fill, -alpha, -stroke)


  #Combining both estimations in a single dataframe
  df_completo <- rbind(data_sunab,data_calsan)


  #Results
  print(summary(est_sunab))
  print(summary(est_calsan))

  df_completo$x <- case_when(df_completo$group == 1 ~ df_completo$x,
                             df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
                             TRUE ~ NA)

  return(df_completo)


  delta_t <- Sys.time() - ini
  print(delta_t)
  rm(delta_t, ini)

}

estimacoes_rais <- calsun_plot(data, plot_title = '', var_y = "rais_")


#Saving the combined dataframe path
calsun <- saveRDS(paste0(data_output_path, "calsun.rds"))


#Preparing the data by assigning colors
calsun <- calsun %>% 
  mutate(
    colour = ifelse( colour == "#145ede","black", "red"),
    x = ifelse(group == 2, x - 0.2, x - 0.2),
    group = ifelse(group == 2, 1, 2)
  )

# ------------------- #
# Borusyak ----
# ------------------- #

res_bjs <- did_imputation(
  data = data,
  yname = "rais_",
  idname = "code_id",
  tname = "ano",
  gname = "year_first_treated",
  first_stage = ~ 1 | code_id + ano_sexo + ano_branco + ano_ensino,
  cluster_var = "code_id",
  horizon = T,
  pretrends = T
)


#Preparing the result data
res_temp <- res_bjs %>% 
  mutate(
    colour = "blue",
    group = 3,
    term = as.numeric(term) + 0.2
  ) %>% 
  rename(
    x = term,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  ) %>% 
  select(-std.error, -lhs) #%>% 

res_temp <- res_temp %>% 
  arrange(colour, x, y, ymin, ymax, group)


#Combining the results in a single dataframe
temp <- rbind(calsun, res_temp)


rm(res_bjs, res_temp)

# -------------------------- #
#De Chaisemartin ----
# -------------------------- #


data$treat_dcm <- ifelse(data$ano < data$year_first_treated, 0, 1)

data$code_id <- as.numeric(data$code_id)

# --------------------------- #
## DISCLAIMER
# ---------------------------------------------------------------------------- #
#' An older version of the data.table package was needed to correctly apply this
#' estimator. The following steps are directed towards this adaptation.
#----------------------------------------------------------------------------- #

install.packages("pkgbuild")
pkgbuild::has_rtools()

install.packages("remotes")
remotes::install_version("data.table", version = "1.16.4", repos = "http://cran.us.r-project.org")

# Estimated @FEA-USP
multiplegt_res <- did_multiplegt_dyn(
  df = data,
  outcome = "rais_",
  group = "year_first_treated",
  time = "ano",
  treatment = "treat_dcm",
  controls = c("ano_sexo", "ano_ensino", "ano_branco", "code_id"),
  effects = 5,
  placebo = 9,
  cluster = "code_id"
)



multiplegt_res$plot

final <- Sys.time()

print(final - ini)

saveRDS(multiplegt_res, paste0(data_output_path, "dechaisemartin.rds"))

chaise <- readRDS(paste0(data_output_path, "dechaisemartin.rds"))


# Joining into the final results dataframe
estimates_df <- chaise$plot$data

estimates_df <- estimates_df %>% 
  mutate(
    colour = "green",
    group = 4,
    Time = as.numeric(Time) - 1.4 #t = 0 Last treatment period
  )  %>% 
  rename(
    x = Time,
    y = Estimate,
    ymin = LB.CI,
    ymax = UB.CI
  )

temp <- rbind(temp, estimates_df)


temp <- temp %>%
  arrange(group)



# ---------------------------------------------------------------------------- #
#2. Graph  ----
# ---------------------------------------------------------------------------- #

p <- ggplot(temp, aes(x = x, y = y, color = colour, shape = colour, group = group)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c('black', 'blue', '#009E60'	, 'red'),
    labels = c("Callaway & Sant'anna",
               "Borusyak et al.",
               "De Chaisemartin & d’Haultfoeuille",
               "Sun & Abraham")
  ) +
  scale_shape_manual(
    values = c(16, 15, 17, 18), # Circle, square, triangle, diamond
    labels = c("Callaway & Sant'anna",
               "Borusyak et al.",
               "De Chaisemartin & d’Haultfoeuille",
               "Sun & Abraham")
  ) +  
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18
    ),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.05, 0.05),         
    legend.justification = c(0, 0)           
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p

ggsave(paste0(data_output_path, "estimators.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave(paste0(data_output_path, "estimators.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


