# ---------------------------------------------------------------------------- #
# Overall Working Paper Code
# Main regression and results
# Last edited by: Tuffy Licciardi Issa
# Date: 25/04/2026
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
library(grid)
library(didimputation)
library(DIDmultiplegt)
library(tidyr)

setwd("C:/Users/tuffy/Documents/IC/")
# ----------------------------------------------------------------------------- #
# 1. DATA
# ----------------------------------------------------------------------------- #

start_time <- Sys.time()

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

# ----------------------- #
#1.1 Result Tables ----
# ----------------------- #

rows <- c("ATT", " ", "Pre-Avg"," ", "Observations")
#' The first step is to create a result dataframe, where the values will be stored.
#' Throughout the code I will be filling the slots to create the final result table
#' for each explored category.

result_rais <- data.frame(
  names = c("ATT", " ", "Pre-Avg"," "),
  att_nc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #No controls
  att_fc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #Full controls
  att_bc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #FC Blue-Collar
  att_wc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," ")))   #FC White-Collar
)

result_cbo <- data.frame(
  names = c("ATT", " ", "Pre-Avg"," "),
  att_nc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #No controls
  att_fc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #Full controls
  att_bc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #FC Blue-Collar
  att_wc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," ")))   #FC White-Collar
)

result_cnae <- data.frame(
  names = c("ATT", " ", "Pre-Avg"," "),
  att_nc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #No controls
  att_fc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #Full controls
  att_bc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," "))),  #FC Blue-Collar
  att_wc = rep(NA, times = length(c("ATT", " ","Pre-Avg"," ")))   #FC White-Collar
)

result_sal <- data.frame(
  names = rows,
  att_nc = rep(NA, times = length(rows)),  #No controls
  att_fc = rep(NA, times = length(rows)),  #Full controls
  att_bc = rep(NA, times = length(rows)),  #FC Blue-Collar
  att_wc = rep(NA, times = length(rows))   #FC White-Colla
  )

rm(rows)
# ---------------------------------------------------------------------------- #
# 2. Main Graphs ----
## 2.1 Function ----
# ---------------------------------------------------------------------------- #
plot <- function(df,
                 plot_title,
                 var_y,
                 controles) {
  
  
  ini <- Sys.time()
  
  var_y <- as.character(substitute(var_y))
  message("Calculando para:", var_y," :)")
  
  
  # Equações com contorles
  sunab_formula <- as.formula(
    paste(
      var_y, "~  sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | code_id + ano_sexo + ano_branco + ano_ensino"
    )
  )
  
  calsan_formula <- as.formula(
    "~ ano_sexo + ano_branco + ano_ensino + code_id "
  )
  
  
  ##Estimações##
  #Sun & Abraham
  est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)
  # Callaway & Sant'anna
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
  
  att_calsan <- est_calsan$overall.att
  att_se     <- est_calsan$overall.se
  
  ##Extraindo os gráficos das duas estimações##
  plot_sunab <- iplot(est_sunab, ref.line = -1,
                      xlab = 'Time to treatment',
                      main = 'Cal_San ES: IGNORAR')  
  
  plot_calsan <- ggdid(est_calsan) +
    ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
    theme_minimal()
  
  ##Extraindo os coeficientes##
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan <- as.data.frame(data_calsan)
  
  data_sunab <- plot_sunab[[1]] 
  data_sunab <- data_sunab %>% 
    mutate(colour = ifelse(id == 1, '#f7200a', NA)) %>% 
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
  
  
  #Unindo os coeficientes das duas estimações
  df_completo <- rbind(data_sunab,data_calsan)
  
  
  
  
  
  #Resultados das estimações
  print(summary(est_sunab))
  print(summary(est_calsan))
  
  df_completo$x <- case_when(df_completo$group == 1 ~ df_completo$x,
                             df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
                             TRUE ~ NA)
  
  

  
  fim <- Sys.time()
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  print("---------------------------------------------")
  print(paste0("Total time elapsed: ",mins," mins e ", secs, " s"))
  print("---------------------------------------------")
  rm(delta, ini, fim, mins, secs)
  
  return(df_completo) #Completed DF
  
}


# ---------------------------------------------------------------------------- #
## 2.2 RAIS ----

estimacoes_rais <- plot(data,
                        plot_title = '',
                        var_y = "rais_")

# if (!dir.exists("C:/Users/tuffy/Documents/IC/Graphs")) {
#   dir.create("C:/Users/tuffy/Documents/IC/Graphs")
# }
# 
# 
# 
# 
# p <- ggplot(estimacoes_rais, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom",
#     panel.grid.minor.x = element_blank()
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c(-0.95, 0.155),
#                      breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
#                      labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
# 
# 
# 
# ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_rais.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
# 






# ---------------------------------------------------------------------------- #
## Removing the SUNAB estimator

#This is the main graph, with the second estimator removed
est_rais2 <- estimacoes_rais %>% 
  filter(group == 2) %>% 
  mutate(x = x - 0.2)

p <- ggplot(est_rais2, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values='black',labels="Callaway & Sant'anna") +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    legend.position = "none",
    
    axis.text.y = element_text(size = 15)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
    labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4'),
    minor_breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4)  
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.155),
    breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )



p



ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais2_v3.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais2_v3.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)




# ---------------------------------------------------------------------------- #
## 2.3 CBO ----

estimacoes_cbo <- plot(data,
                       plot_title = '',
                       var_y = "dummy_cbo")


p <- ggplot(estimacoes_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 18) +
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
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 0.95),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90'))


ggsave("Graphs/plot_cbo.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)




rm(p)

# ---------------------------------------------------------------------------- #
## 2.4 CNAE ----

estimacoes_cnae <- plot(data,
                        plot_title = '',
                        var_y = "dummy_cnae")


p <- ggplot(estimacoes_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    legend.position = "none",
    
    axis.text.y = element_text(size = 15)
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90,1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90','1.05'))



ggsave("Graphs/plot_cnae.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)




rm(p)
gc()
# ---------------------------------------------------------------------------- #
## 2.5 Salary -----
# ---------------------------------------------------------------------------- #

estimacoes_sal <- plot(data %>% 
                         group_by(code_id) %>%
                         filter(all(cnae_group[ano < year_first_treated] ==
                                      cnae_pre_treat[ano < year_first_treated]),
                                all_in_rais == 1) %>%
                         ungroup(),
                        plot_title = '',
                        var_y = "remuneracao_media_sm_")


p <- ggplot(estimacoes_sal %>% #Only Callaway
              filter(group == 2) %>% 
              mutate(x = x - 0.2),
            aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values='black',labels="Callaway & Sant'anna") +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    legend.position = "none",
    
    axis.text.y = element_text(size = 15)
  ) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
    labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4'),
    minor_breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4)  
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.45),
    breaks = seq(-0.6, 0.45, by = 0.15),
    labels = sprintf("%.2f", seq(-0.6, 0.45, by = 0.15))
  )

p

ggsave("Graphs/final/plot_sal.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)





# ---------------------------------------------------------------------------- #
# 3. White vs. Blue ----
# ---------------------------------------------------------------------------- #
## 3.1 Data Frames ----


blue_data <- data %>% 
  filter(white_dummy == 0)

white_data <- data %>% 
  filter(white_dummy == 1)

# ---------------------------------------------------------------------------- #
## 3.2 Blue Collar ----
### 3.2.1 RAIS ----
estimacoes_brais <- plot(blue_data,
                         plot_title = '',
                         var_y = "rais_")

# 
# p <- ggplot(estimacoes_brais, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c(-0.95, 0.155),
#                      breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
#                      labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
# 
# 
# 
# p
# 
# ggsave("Graphs/plot_rais_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
# 

# ---------------------------------------------------------------------------- #
### 3.2.2 CBO ----
estimacoes_bcbo <- plot(blue_data,
                        plot_title = '',
                        var_y = "dummy_cbo")
# 
# 
# p <- ggplot(estimacoes_bcbo, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c( -0.155, 0.95),
#                      breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90),
#                      labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90'))
# 
# 
# 
# ggsave("Graphs/plot_cbo_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ----------------------------------------------------------------------------- #
### 3.2.3 CNAE ----
estimacoes_bcnae <- plot(blue_data,
                         plot_title = '',
                         var_y = "dummy_cnae")

# 
# p <- ggplot(estimacoes_bcnae, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c( -0.155, 1.10),
#                      breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
#                      labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))
# 
# 
# 
# ggsave("Graphs/plot_cnae_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ---------------------------------------------------------------------------- #
### 3.2.4 Salary ----
# ---------------------------------------------------------------------------- #

estimacoes_bsal <- plot(blue_data %>% 
                         group_by(code_id) %>%
                         filter(all(cnae_group[ano < year_first_treated] ==
                                      cnae_pre_treat[ano < year_first_treated]),
                                all_in_rais == 1) %>%
                         ungroup(),
                       plot_title = '',
                       var_y = "remuneracao_media_sm_")


# ---------------------------------------------------------------------------- #
## 3.3 White Collar ----
# ---------------------------------------------------------------------------- #
### 3.3.1 RAIS --------------
estimacoes_wrais <- plot(white_data,
                         plot_title = '',
                         var_y = "rais_")

# 
# p <- ggplot(estimacoes_wrais, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c(-0.95, 0.155),
#                      breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
#                      labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
# 
# 
# 
# ggsave("Graphs/plot_rais_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
# 



# ---------------------------------------------------------------------------- #
### 3.3.2 CBO --------------
# ---------------------------------------------------------------------------- #

estimacoes_wcbo <- plot(white_data,
                        plot_title = '',
                        var_y = "dummy_cbo")

# 
# p <- ggplot(estimacoes_wcbo, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c( -0.155, 1.10),
#                      breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
#                      labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))
# 
# 
# 
# ggsave("Graphs/plot_cbo_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
# 

# ---------------------------------------------------------------------------- #
### 3.3.3 CNAE --------------
# ---------------------------------------------------------------------------- #

estimacoes_wcnae <- plot(white_data,
                         plot_title = '',
                         var_y = "dummy_cnae"
)


# p <- ggplot(estimacoes_wcnae, aes(x = x, y = y, color = colour, group = group)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
#   geom_hline(yintercept = 0, color = "#D62728") +
#   geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
#   scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
#   labs(x = "Years to treatment", y = '', colour = '') +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_line(),
#     axis.line.y = element_line(),
#     legend.position = "bottom"
#   ) +
#   scale_x_continuous(limits = c(-9.2, 5.2),
#                      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
#                      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
#   scale_y_continuous(limits = c( -0.155, 1.10),
#                      breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
#                      labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))
# 
# 
# 
# ggsave("Graphs/plot_cnae_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ---------------------------------------------------------------------------- #
### 3.3.4 Salary --------------
# ---------------------------------------------------------------------------- #

estimacoes_wsal <- plot(white_data %>% 
                         group_by(code_id) %>%
                         filter(all(cnae_group[ano < year_first_treated] ==
                                      cnae_pre_treat[ano < year_first_treated]),
                                all_in_rais == 1) %>%
                         ungroup(),
                       plot_title = '',
                       var_y = "remuneracao_media_sm_")

# ---------------------------------------------------------------------------- #
# 4. New Spec (WxB)----
# ---------------------------------------------------------------------------- #
## 4.1 Data ----
# ---------------------------------------------------------------------------- #
### 4.1.1 RAIS ----
# ---------------------------------------------------------------------------- #

estimacoes_brais$collar = 0#Blue
estimacoes_wrais$collar = 1
#União das Bases
both_rais <- rbind(estimacoes_brais, estimacoes_wrais)

#Escolhendo os estimadores CALSAN
both_rais <- both_rais %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    x = ifelse(collar == 1, x-0.2, x),
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(
    -collar
  )

# ----------------------------------------------------------------------------- #
### 4.1.2 CBO ----
# ----------------------------------------------------------------------------- #


estimacoes_bcbo$collar = 0#Blue
estimacoes_wcbo$collar = 1
#União das Bases
both_cbo <- rbind(estimacoes_bcbo, estimacoes_wcbo)

#Escolhendo os estimadores CALSAN
both_cbo <- both_cbo %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    
    x = ifelse(collar == 1, x-0.2, x),
    
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(-collar)

# ----------------------------------------------------------------------------- #
### 4.1.3 CNAE ----
# ----------------------------------------------------------------------------- #

estimacoes_bcnae$collar = 0#Blue
estimacoes_wcnae$collar = 1
#União das Bases
both_cnae <- rbind(estimacoes_bcnae, estimacoes_wcnae)

#Escolhendo os estimadores CALSAN
both_cnae <- both_cnae %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    
    x = ifelse(collar == 1, x-0.2, x),
    
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(-collar)

# ----------------------------------------------------------------------------- #
### 4.1.4 Salary ----
#'Joining salary estimation data.
# ----------------------------------------------------------------------------- #
estimacoes_bsal$collar = 0#Blue
estimacoes_wsal$collar = 1

both_sal <- rbind(estimacoes_bsal, estimacoes_wsal)

both_sal <- both_sal %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    
    x = ifelse(collar == 1, x-0.2, x),
    
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(-collar)

# ----------------------------------------------------------------------------- #
### 4.1.5 Saving ----
# ----------------------------------------------------------------------------- #
# saveRDS(estimacoes_rais, "C:/Users/tuffy/Documents/IC/Bases/results_est/rais_total.RDS")
# saveRDS(both_rais, "C:/Users/tuffy/Documents/IC/Bases/results_est/rais_both_wc.RDS")
# saveRDS(both_cbo, "C:/Users/tuffy/Documents/IC/Bases/results_est/cbo_both_wc.RDS")
# saveRDS(both_cnae, "C:/Users/tuffy/Documents/IC/Bases/results_est/cnae_both_wc.RDS")
# saveRDS(both_sal, "C:/Users/tuffy/Documents/IC/Bases/results_est/sal_both_wc.RDS")




# ---------------------------------------------------------------------------- #
## 4.2 Estimation ----
### 4.2.1 RAIS ----
# ----------------------------------------------------------------------------- #
#both_rais <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/rais_both_wc.RDS")


p <- ggplot(both_rais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    axis.text.y = element_text(size = 15
    ),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.05, 0.05),         
    legend.justification = c(0, 0)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

#  --------------------------------------------------------------------------- #
### 4.2.2 CBO ----
# ----------------------------------------------------------------------------- #


#both_cbo <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/cbo_both_wc.RDS")

p <- ggplot(both_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    axis.text.y = element_text(size = 15),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),         
    legend.justification = c(1, 1)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))



p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cbo_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cbo_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
### 4.2.3 CNAE ----
# ----------------------------------------------------------------------------- #

#both_cnae <- readRDS( "C:/Users/tuffy/Documents/IC/Bases/results_est/cnae_both_wc.RDS")


p <- ggplot(both_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 15),
    axis.text.y = element_text(size = 15),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),         
    legend.justification = c(1, 1)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))

p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cnae_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cnae_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


# ---------------------------------------------------------------------------- #
### 4.2.4 Salary ----
# ---------------------------------------------------------------------------- #

p <- ggplot(both_sal, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 0.25),         
    legend.justification = c(1, 1)
    ) +           
    scale_x_continuous(
      limits = c(-9.5, 4.5),
      breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
      labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4'),
      minor_breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4)  
    ) +
      scale_y_continuous(
        limits = c(-0.6, 0.45),
        breaks = seq(-0.6, 0.45, by = 0.15),
        labels = sprintf("%.2f", seq(-0.6, 0.45, by = 0.15))
      )

p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/final/plot_sal_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/final/pdf/plot_sal_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


# ---------------------------------------------------------------------------- #
# 5. More analysis ----
# ---------------------------------------------------------------------------- #

stargazer(data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_all",
          file = "C:/Users/tuffy/Documents/IC/Tables/total_desc.tex")

#Repetir para BC e WC

stargazer(blue_data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_blue",
          file = "C:/Users/tuffy/Documents/IC/Tables/blue_desc.tex")

stargazer(white_data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_white",
          file = "C:/Users/tuffy/Documents/IC/Tables/white_desc.tex")

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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_nc[1] <- paste0(round(att, 4), stars)
result_rais$att_nc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 6.1.2 With Controls -----
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

est_calsan_sc1 <- aggte( MP = w_control, type = "dynamic", na.rm = TRUE)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_fc[1] <- paste0(round(att, 4), stars)
result_rais$att_fc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE

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
  data =data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

# ----------------------------------------------------------------------------- #
### 6.2.2 Table Both ----
# ---------------------------------------------------------------------------- #


est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_nc[1] <- paste0(round(att, 4), stars)
result_cbo$att_nc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE


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

est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_nc[1] <- paste0(round(att, 4), stars)
result_cnae$att_nc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
## 6.4 Salary ----
# ---------------------------------------------------------------------------- #
### 6.4.1 No Controls ----
# ---------------------------------------------------------------------------- #


no_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

# ----------------------------------------------------------------------------- #
#### 6.4.1.1 Table No Controls----
# ----------------------------------------------------------------------------- #

est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))



#Saving in the result table
result_sal$att_nc[1] <- paste0(round(att, 4), stars)
result_sal$att_nc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE
result_sal$att_nc[5] <- nrow(data %>% 
       group_by(code_id) %>%
       filter(all(cnae_group[ano < year_first_treated] ==
                    cnae_pre_treat[ano < year_first_treated]),
              all_in_rais == 1) %>%
       ungroup()) #Observations
# ----------------------------------------------------------------------------- #
### 6.4.2 With Controls ----
# ----------------------------------------------------------------------------- #

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id, 
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
  )


est_calsan_sc1 <- aggte( MP = w_control, type = "dynamic", na.rm = TRUE)
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se
z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_sal$att_fc[1] <- paste0(round(att, 4), stars)
result_sal$att_fc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE
result_sal$att_fc[5] <- nrow(data %>% 
       group_by(code_id) %>%
       filter(all(cnae_group[ano < year_first_treated] ==
                    cnae_pre_treat[ano < year_first_treated]),
              all_in_rais == 1) %>%
       ungroup()) #Observations

rm(w_control)

# ---------------------------------------------------------------------------- #
## 6.5 Blue Collar ----
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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_bc[1] <- paste0(round(att, 4), stars)
result_rais$att_bc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
### 6.5.2 CBO BC ----
# ----------------------------------------------------------------------------- #

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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_bc[1] <- paste0(round(att, 4), stars)
result_cbo$att_bc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
### 6.5.3 CNAE BC ----
# ----------------------------------------------------------------------------- #


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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_bc[1] <- paste0(round(att, 4), stars)
result_cnae$att_bc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE

# ----------------------------------------------------------------------------- #
### 6.5.4 Salary BC ----
# ----------------------------------------------------------------------------- #

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = w_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = w_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)


att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_sal$att_bc[1] <- paste0(round(att, 4), stars)
result_sal$att_bc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE
result_sal$att_bc[5] <- nrow(blue_data %>% 
       group_by(code_id) %>%
       filter(all(cnae_group[ano < year_first_treated] ==
                    cnae_pre_treat[ano < year_first_treated]),
              all_in_rais == 1) %>%
       ungroup()) #Observations

rm(att, se, z_value, p_value, stars)

# ---------------------------------------------------------------------------- #
## 6.6 White Collar ----
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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_wc[1] <- paste0(round(att, 4), stars)
result_rais$att_wc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 6.6.2 CBO WC ----
# ----------------------------------------------------------------------------- #

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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_wc[1] <- paste0(round(att, 4), stars)
result_cbo$att_wc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
### 6.6.3 CNAE WC ----
# ----------------------------------------------------------------------------- #


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



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

#Saving values
att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_wc[1] <- paste0(round(att, 4), stars)
result_cnae$att_wc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE

# ----------------------------------------------------------------------------- #
### 6.6.4 Salary WC ----
# ----------------------------------------------------------------------------- #

w_control <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_sc1 <- aggte( MP = w_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = w_control, type = "simple", na.rm = T)

att <- est_calsan_sc1$overall.att
se  <- est_calsan_sc1$overall.se

z_value <- att / se
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_sal$att_wc[1] <- paste0(round(att, 4), stars)
result_sal$att_wc[2] <- paste0("[",round(est_calsan_sc1$overall.se, digits = 4),"]") #SE
result_sal$att_wc[5] <- nrow(white_data %>% 
       group_by(code_id) %>%
       filter(all(cnae_group[ano < year_first_treated] ==
                    cnae_pre_treat[ano < year_first_treated]),
              all_in_rais == 1) %>%
       ungroup()) #Observations

rm(att, se, z_value, p_value, stars)

# ---------------------------------------------------------------------------- #
# 7. Pre-Avg ----
# ---------------------------------------------------------------------------- #
## 7.1 RAIS ----
### 7.1.1 With Controls -----
# ----------------------------------------------------------------------------- #

#RAIS - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)



## Extraindo a V ----#

es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)

# Create LaTeX row string

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_fc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_rais$att_fc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.1.2 No Controls ----
# ---------------------------------------------------------------------------- #

#RAIS - Sem Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)


#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)


# Create LaTeX row string

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_nc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_rais$att_nc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
## 7.2 CBO ----
### 7.2.1 With Controls -----
# ---------------------------------------------------------------------------- #

#CBO - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)


# Create LaTeX row string

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_fc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cbo$att_fc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE



# ---------------------------------------------------------------------------- #
### 7.2.2 No Controls -----
# ---------------------------------------------------------------------------- #

#CBO - Sem Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)

# Create LaTeX row string

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_nc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cbo$att_nc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE


# ---------------------------------------------------------------------------- #
## 7.3 CNAE ----
### 7.3.1 With Controls -----
# ---------------------------------------------------------------------------- #

#CNAE - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)


#Creating the variable for the LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_fc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cnae$att_fc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.3.2 No Controls ----
# ---------------------------------------------------------------------------- #

#CNAE - Sem Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_nc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cnae$att_nc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE


# ----------------------------------------------------------------------------- #
## 7.4 Salary ----
# ---------------------------------------------------------------------------- #
### 7.4.1 With Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_sal$att_fc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_sal$att_fc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

rm(pre_av, se_equal, z_value, p_value, stars)

# ---------------------------------------------------------------------------- #
### 7.4.2 No Controls ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)
# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0
# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n

## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_sal$att_nc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_sal$att_nc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE
rm(pre_av, se_equal, z_value, p_value, stars)

# ---------------------------------------------------------------------------- #
## 7.4 Blue Collar ----
### 7.4.1 RAIS ----
# ---------------------------------------------------------------------------- #

#RAIS - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_bc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_rais$att_bc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.4.2 CBO ----
# ---------------------------------------------------------------------------- #

#CBO - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_bc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cbo$att_bc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.4.3 CNAE ----
# ---------------------------------------------------------------------------- #

#CNAE - Controles
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)


# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_bc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cnae$att_bc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
###7.4.4 Salary - Controles ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)
# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0
# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n
## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]
## 2) equal weights
w <- rep(1/8, 8)
## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))
## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)
## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))
#Saving in the result table
result_sal$att_bc[3] <- paste0(round(pre_av, 4), stars ) #ATT

result_sal$att_bc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE
rm(pre_av, se_equal, z_value, p_value, stars)

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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_rais$att_wc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_rais$att_wc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.5.2 CBO -----
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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)


# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cbo$att_wc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cbo$att_wc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

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

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string
z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))

#Saving in the result table
result_cnae$att_wc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_cnae$att_wc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE

# ---------------------------------------------------------------------------- #
### 7.5.4 Salary - Controles ----
# ---------------------------------------------------------------------------- #

pre_avg <- did::att_gt(
  yname = "remuneracao_media_sm_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data %>% 
    group_by(code_id) %>%
    filter(all(cnae_group[ano < year_first_treated] ==
                 cnae_pre_treat[ano < year_first_treated]),
           all_in_rais == 1) %>%
    ungroup(),
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)
# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0
#Calculating the mean effect
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n
## 1) take the 8x8 block from V (rows/cols 1:8 as shown)
V8 <- V[1:8, 1:8]
## 2) equal weights
w <- rep(1/8, 8)
## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)
## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the
# value without sqrt
se_equal   # optional: the average SE (with sqrt)

z_value <- pre_av / se_equal
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.10, "*", "")))
#Saving in the result table
result_sal$att_wc[3] <- paste0(round(pre_av, 4), stars) #ATT
result_sal$att_wc[4] <- paste0("[",round(se_equal, digits = 4),"]") #SE
rm(pre_av, se_equal, z_value, p_value, stars)



# ---------------------------------------------------------------------------- #
# 8. Final Tables ----
# ---------------------------------------------------------------------------- #

loop <- c("rais","cbo","cnae", "sal")

for (name in loop) {
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    get(paste0("result_", name)),
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, paste0("C:/Users/tuffy/Documents/IC/Tables/",name,"_main.tex"))
  
  rm(latex_table, name)
    
}
rm(loop)


# ---------------------------------------------------------------------------- #
# Total elapsed time

final_time <- Sys.time()

delta <- difftime(final_time, start_time, units = "secs")

mins <- floor(as.numeric(delta) / 60)
secs <- round(as.numeric(delta) %% 60)
hours <- floor(as.numeric(mins)/ 60)


message("---------------------------------------------")
message("Total time elapsed: ", hours, " hours, ",mins," mins and ", secs, " s")
message("---------------------------------------------")
rm(list = ls())

