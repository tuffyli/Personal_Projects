# ---------------------------------------------------------------------------- #
# Robustsness and Heterogeneity tests
# Aditional tests
# Last edited by: Tuffy Licciardi Issa
# Date: 20/11/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #
library(dplyr)
library(fixest)
library(ggplot2)
library(did)
library(stargazer)
library(gridExtra)
library(knitr)
library(grid)

# --------------------------------------------------------------------------- #
#Data Base ----
# --------------------------------------------------------------------------- #

base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")

#Marking the individuals who appear in RAIS in all periods
base <- base %>%   
  group_by(code_id) %>% 
  mutate( all_in_rais = min(rais_)
  ) %>% 
  select(
    -raca_aux,
    -sexo_aux,
    -ensino_aux,
    -sexo_initial,
    -escolaridade_initial,
    -raca_initial,
    -sexo_,
    -escolaridade_,
    -raca_,
    -rais_aux2,
    -max_aux2,
  ) %>% 
  filter(
    all_in_rais == 1
  )

# ---------------------------------------------------------------------------- #
#1. Salary -------
# ---------------------------------------------------------------------------- #
# ------------- #
## 1.1 Data ----
# ------------- #
summary(base$remuneracao_media_sm_)

base <- base %>%
  group_by(code_id) %>% 
  mutate(
    sal_var = log(1+remuneracao_media_sm_),
    
    
    sal_var_t0_aux = ifelse(ano == year_first_treated, sal_var, NA),
    sal_var_t0_aux2 = max(sal_var_t0_aux, na.rm = T),
    sal_var_t0 = sal_var - sal_var_t0_aux2,
    
    sal_var_t2_aux = ifelse(ano == year_first_treated - 2, sal_var, NA),
    sal_var_t2_aux2 = max(sal_var_t2_aux, na.rm = T),
    sal_var_t2 = sal_var - sal_var_t2_aux2,
    
    
    sal_var_tmean_aux = ifelse( ano %in% c(year_first_treated - 2, year_first_treated -1, year_first_treated),
                                sal_var, NA),
    sal_var_tmean_aux2 = mean(sal_var_tmean_aux, na.rm = T),
    sal_var_tmean = sal_var - sal_var_tmean_aux2
  ) %>%
  ungroup() %>% 
  select(
    -c(sal_var_t0_aux, sal_var_t0_aux2,
       sal_var_t2_aux, sal_var_t2_aux2,
       sal_var_tmean_aux, sal_var_tmean_aux2)
  )

base$cbo_ano <- as.numeric(interaction(base$cbo_group, base$ano))
base$cnae_ano <- as.numeric(interaction(base$cnae_group, base$ano))

base$minus_1 <- base$year_first_treated - 1
base$minus_2 <- base$year_first_treated - 2

# ------------- #
## 1.2 Estimation ----
# ------------- #

# ------------- #
###1.2.1 No FE ----
# ------------- #
####1.2.1.1 T0 -----
#Here I utilized the real treatment timming
#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t0 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t0

#### 1.2.1.2 T2 -----
#'The results in T0 promped me to test for a tratment timming 2 periods before the real treatment
#'this is a direct resul from the labor lawsuits strutures in Brazil, where the
#'plaintiff workers has a 2 year period to start the lawsuit after the termination

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_t2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Results table
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t2


#### 1.2.1.3 T MEAN -----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_tmean",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_tmean <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_tmean


###1.2.2 New Spec ----

#' The new specification removed all observations where the observed salary was
#' equal to 0.

base2 <- base %>%
  group_by(code_id) %>%
  filter(all(remuneracao_media_sm_ != 0)) %>%
  mutate(
    sal_var = log(remuneracao_media_sm_)
  )


#### 1.2.2.1 T0 ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base2,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t0 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t0


#### 1.2.2.2 T2 ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var_t2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t2


#### 1.2.2.3 T MEAN ----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_tmean",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_tmean <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_tmean




##1.3 With FE ----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan




###1.3.1 -1 period ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "minus_1",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-1) ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


###1.3.2 -2 period ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "minus_2",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-2) ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan



# 
# ##Extraindo os coeficientes##
# data_calsan <- ggplot_build(plot_calsan)$data[[1]]
# data_calsan <- as.data.frame(data_calsan)
# 
# 
# data_calsan <- data_calsan %>% 
#   mutate(colour = '#145ede',
#          group = 1,
#          y = ifelse(x == -1, 0, y),
#          ymin = ifelse(x == -1, 0, ymin),
#          ymax = ifelse(x == -1, 0, ymax)) %>% 
#   select(-PANEL, -shape, -size, -fill, -alpha, -stroke)

rm(base)

# -----------------------------------------------------------------------------#
# 2. Sex ----
# -----------------------------------------------------------------------------#

# ----------- #
## 2.0 Data -----
# ----------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data$cbo_ano <- as.numeric(interaction(data$cbo_group, data$ano))
data$cnae_ano <- as.numeric(interaction(data$cnae_group, data$ano))

#Filtering the database according to each individual sex
base_mas <- data %>% 
  filter(sexo_dummy == 1) #male

base_fem <- data %>% 
  filter(sexo_dummy == 0 ) #female

#depois subdividir em white e blue collar

## 2.1 Female ----
#Estimating
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Extracting
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 2.2 Male ----
#Estimating
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the results for the female workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##2.3 Combined ----
#The combined results database

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##2.4 White-collar ----

### 2.4.1 Female ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 1) 
  

base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 1)

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan

#Extracting the results for the female workers
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 2.4.2 Male ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Final results for the male workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 2.4.3 Combined  ----
# Combining the results databases
data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Final Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_wc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_wc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##2.5 Blue-collar ----

### 2.5.1 Female ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 0) 


base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 0)

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Extracting the results for the female workers
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 2.5.2 Male ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the results for the male workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 2.5.3 Combined  ----
#The combined results database
data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_bc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_bc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



# -----------------------------------------------------------------------------#
# 3. Race/Color ----
# -----------------------------------------------------------------------------#

# ----------- #
## 3.0 Data -----
# ----------- #

#Filtering the database for individuals race/color
base_bra <- data %>% 
  filter(branco_dummy == 1) #white

base_nbr <- data %>% 
  filter(branco_dummy == 0 )#non-white

#separating into further white and blue collars

## 3.1 White----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Final result dataframe for Whites
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 3.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the final results for Non-Whites
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##3.3 Combined  ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##3.4 White-collar ----

### 3.4.1 White ----
#Creating the segmentated auxiliar databases
base_bra <- data %>% 
  filter(branco_dummy == 1, #white
         white_dummy == 1)  #white-collar


base_nbr <- data %>% 
  filter(branco_dummy == 0, #non-white
         white_dummy == 1) #white-collar

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Final results dataframe
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 3.4.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2


#Final result dataframe
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 3.4.3 Combined  ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_wc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_wc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##3.5 Blue-collar ----

### 3.5.1 White ----

#Creating the databases
base_bra <- data %>% 
  filter(branco_dummy == 1, #White
         white_dummy == 0)  #Blue-collar


base_nbr <- data %>% 
  filter(branco_dummy == 0, #Non-white
         white_dummy == 0) #Blue-collar

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan

#Creating the final results dataframe
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 3.5.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Creating the final results dataframe
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 3.5.3 Combined ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
  labs(x = "Years to treatment", y = '', colour = '') +
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
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_bc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_bc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)





# -----------------------------------------------------------------------------#
# 4. Estimators ----
# -----------------------------------------------------------------------------#

rm(base)

# install.packages("didimputation")
# install.packages("DIDmultiplegt")




library(didimputation)
library(DIDmultiplegt)



data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


## ------------------------- #
## 4.1 CalSan & SunAb ----
##-------------------------- #
##' Here I estimate the results for the main estimators I employed, being the ones
##' developed by Callaway and Sant'anna, and Sun and Abraham.
#
# plot <- function(df,
#                  plot_title,
#                  var_y,
#                  controles
# ) {
#   
#   
#   ini <- Sys.time()
#   
#   print(paste0("Calculando para:", var_y," :)"))
#   
#   var_y <- as.character(substitute(var_y))
#   
#   # Equações com contorles
#   sunab_formula <- as.formula(
#     paste(
#       var_y, "~  sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | code_id + ano_sexo + ano_branco + ano_ensino"
#     )
#   )
#   
#   calsan_formula <- as.formula(
#     "~ ano_sexo + ano_branco + ano_ensino + code_id "
#   )
#   
#   
#   ##Estimações##
#   #Sun & Abraham
#   est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)
#   # Callaway & Sant'anna
#   calsan_did <- did::att_gt(
#     yname = var_y,
#     gname = "year_first_treated",
#     idname = "code_id",
#     tname = "ano",
#     xformla = calsan_formula,
#     data = df,
#     control_group = "notyettreated",
#     base_period = "universal",
#     clustervars = "code_id"
#   )
#   est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#   print(est_calsan)
#   
#   ##Extraindo os gráficos das duas estimações##
#   plot_sunab <- iplot(est_sunab, ref.line = -1,
#                       xlab = 'Time to treatment',
#                       main = 'Cal_San ES: IGNORAR')  
#   
#   plot_calsan <- ggdid(est_calsan) +
#     ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
#     labs("Time to treat") +
#     theme_minimal()
#   
#   ##Extraindo os coeficientes##
#   data_calsan <- ggplot_build(plot_calsan)$data[[1]]
#   data_calsan <- as.data.frame(data_calsan)
#   
#   data_sunab <- plot_sunab[[1]] 
#   data_sunab <- data_sunab %>% 
#     mutate(colour = ifelse(id == 1, '#f7200a')) %>% 
#     rename(
#       ymin = ci_low,
#       ymax = ci_high,  
#       group = id ) %>% 
#     select(colour, x, y, ymin, ymax, group,-estimate, -estimate_names, -estimate_names_raw,-is_ref)
#   
#   data_calsan <- data_calsan %>% 
#     mutate(colour = '#145ede',
#            group = 2,
#            y = ifelse(x == -1, 0, y),
#            ymin = ifelse(x == -1, 0, ymin),
#            ymax = ifelse(x == -1, 0, ymax)) %>% 
#     select(-PANEL, -shape, -size, -fill, -alpha, -stroke)
#   
#   
#   #Unindo os coeficientes das duas estimações
#   df_completo <- rbind(data_sunab,data_calsan)
#   
#   
#   
#   
#   
#   #Resultados das estimações
#   print(summary(est_sunab))
#   print(summary(est_calsan))
#   
#   df_completo$x <- case_when(df_completo$group == 1 ~ df_completo$x,
#                              df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
#                              TRUE ~ NA)
#   
#   return(df_completo)
#   
#   
#   delta_t <- Sys.time() - ini
#   print(delta_t)
#   rm(delta_t, ini)
#   
# }
# 
# estimacoes_rais <- plot(data,
#                         plot_title = '',
#                         var_y = "rais_")


calsun <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/rais_total.RDS")

calsun <- calsun %>% 
  mutate(
    colour = ifelse( colour == "#145ede","black", "red"),
    x = ifelse(group == 2, x - 0.2, x - 0.2),
    group = ifelse(group == 2, 1, 2)
    )

# ------------------- #
## 4.2 Borusyak ----
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
  


temp <- rbind(calsun, res_temp)


rm(res_bjs, res_temp)

# -------------------------- #
## 4.3 De ChaiseMartin ----
# -------------------------- #


data$treat_dcm <- ifelse(data$ano < data$year_first_treated, 0, 1)

# data$code_id <- as.numeric(data$code_id)
# 
# multiplegt_res <- did_multiplegt_old(
#   df = data, 
#   Y = "rais_",
#   G = "year_first_treated",
#   T = "ano",
#   D = "treat_dcm",
#   #i = "code_id",
#   controls = c("code_id","ano_sexo", "ano_branco", "ano_ensino"),
#   dynamic = 5,
#   placebo = 5,
#   brep = 50,
#   #parallel = T,
#   #cluster = "code_id"
#   #,
#   # homogeneous_att = FALSE,
#   # mode = "old"
# )

## --------------------------- #
### DISCLAIMER
## --------------------------- #
##' An older version of the data.table package was needed to correctly apply this
##' estimator. The following steps are directed towards this adaptation.
##----------- #
#
# install.packages("pkgbuild")
# pkgbuild::has_rtools()
# 
# install.packages("remotes")
# remotes::install_version("data.table", version = "1.16.4", repos = "http://cran.us.r-project.org")
#
## Estimated @FEA-USP
# multiplegt_res <- did_multiplegt_dyn(
#   df = data,
#   outcome = "rais_",
#   group = "year_first_treated",
#   time = "ano",
#   treatment = "treat_dcm",
#   controls = c("ano_sexo", "ano_ensino", "ano_branco", "code_id"),
#   effects = 5,
#   placebo = 9,
#   cluster = "code_id"
# )
# 
# 
# 
# multiplegt_res$plot
# 
# final <- Sys.time()
# 
# print(final - ini)
# 
# saveRDS(multiplegt_res, "C:/Users/tuffy/Documents/IC/Bases/results_est/dechaisemartin.RDS")

chaise <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/dechaisemartin.RDS")


# Joining into the final results dataframe
estimates_df <- chaise$plot$data

estimates_df <- estimates_df %>% 
  mutate(
    colour = "green",
    group = 4,
    Time = as.numeric(Time) - 1.4 #t = 0 Último período em que não é tratado.
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
## 4.4 Graph  ----
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

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/estimators.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/estimators.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)



# ---------------------------------------------------------------------------- #
# 5. New variable tests ----
# ---------------------------------------------------------------------------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data <- data %>% 
  filter(all_in_rais == 1) #presence in all periods

data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    first_cnae = cnae_group[ano == 2003]
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>% 
  ungroup()

# --------------------- #
## 5.1 CNAE ------ 
# --------------------- #


#' The CNAE variable can more easily capture movement between firms, since their
#' are naturally assigned into different codes. Thus, this is estimations tried 
#' to capture the true firm change movement. Differently from previous estimations
#' this section is directed to capturing the CNAE code when compared to the first
#' CNAE observation of each individual in the dataset.

summary(data$cnae_group)

#Steps for dummy criation
treat <- data %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_,
    first_cnae
  ) %>%
  group_by(code_id) %>% 
  
  mutate(
    #Auxiliatory Dummy - free movement after treatment
    dummy_cnae_aux =        
      case_when(
        #Possible cases
        ano < year_first_treated & cnae_group != first_cnae ~ 1, 
        
        #ano >= year_first_treated & cnae_group == 0 ~ 1,
        
        ano >= year_first_treated & cnae_group != 0 & !(cnae_group %in% cnae_group[ano < year_first_treated]) ~ 1,
        
        TRUE ~ 0
      ),
    #Final dummy
    first_one = which(dummy_cnae_aux == 1 & ano >= year_first_treated)[1], #First pre-treatment row
    
    dummy_cnae_all = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, dummy_cnae_aux
    ),
    
    dummy_cnae_all = ifelse(
      is.na(first_one) & ano >= year_first_treated, 0 , dummy_cnae_all #Condition to deal with the cases where there is no first one
    ),
    
    new_dummy_cnae = ifelse(
      cnae_group != first_cnae, 1, 0
    )
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all, new_dummy_cnae #Selecting only the merge necessary collumns
  )

#Joinning both dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)

# ----------------- #
## 5.2 Estimation ----
# ------------------ #

calsan_did <- did::att_gt(
  yname = "new_dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - all_in_rais ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

# --------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# 6. Newest CNAE variable ----
# ---------------------------------------------------------------------------- #
#dataset
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cnae_group == 0 & ano < year_first_treated)[1],
    aux2 = which(cnae_group != 0 & ano < year_first_treated)[1]

  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup()



treat <- data %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_,
    aux2,
    aux1
  ) %>%
  group_by(code_id) %>% 
  
  mutate(
    cnae_aux = cnae_group[row_number() == aux2],
    cnae_dummy_v2 = ifelse(cnae_group != cnae_aux & cnae_group != 0, 1, 0),
    
    
    first_one = which(cnae_dummy_v2 == 1 & ano >= year_first_treated)[1], #First row pre-treatment
    
    dummy_cnae_all_v2 = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, cnae_dummy_v2
    ),
    
    
    #Accountig for INF values
    cnae_dummy_v3 = ifelse(cnae_group != cnae_aux, 1, 0),
    
    cnae_dummy_v4 = ifelse(cnae_group != cnae_aux, 1, 0), #direct dummy
    
    
    
    first_one2 = which(cnae_dummy_v3 == 1 & ano >= year_first_treated)[1], #First row pre-treat
    
    dummy_cnae_all_v3 = ifelse(
      row_number() >= first_one2 & !is.na(first_one2), 1, cnae_dummy_v3 #propagating the result through periods
    ),
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all_v2, dummy_cnae_all_v3, cnae_dummy_v4 #Only necessary collumns
  )

#Combining the dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)


#'Next we will comapre the results found between the two main dummy specifications

# ------------------------- #
## 6.2 D2 Estimation ----
# ------------------------- #

calsan_did <- did::att_gt(
  yname = "dummy_cnae_all_v2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança CNAE ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan
ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test2_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

# -------------------------- #
## 6.2 D4*** Estimation ----
# -------------------------- #
#' This estimation utilized the 4 dummy spec. The stars refered to the prefered 
#' specification for the code.

#Aqui só utilizamos a variável de CNAE da primeira observação.

calsan_did <- did::att_gt(
  yname = "cnae_dummy_v4",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança  v2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test5_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

rm(treat)

# ---------------------------------------------------------------------------- #
# mun ====
# ----------------------------------------------------------------------------#


# ------------------------- #
## 6.2 D2 Estimation ----
# ------------------------- #

calsan_did <- did::att_gt(
  yname = "simple_mun_dummy",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Mun - Mudança Mun ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan
ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test1_mun.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)






#------------------------------------------------------------------------------#
# 7. Honest DID ----
# -----------------------------------------------------------------------------#

# # Install remotes package if not installed
# install.packages("remotes")
# 
# # Turn off warning-error-conversion, because the tiniest warning stops installation
# Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
# 
# # install from github
# remotes::install_github("asheshrambachan/HonestDiD")

library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)

# ---------------- #
## 7.1 Function ----


#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#'
#' @param ... Parameters to pass to the relevant method.
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param es Result from aggte (object of class AGGTEobj).
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @param gridPoints Number of grid points used for the underlying test
#'  inversion. Default equals 100. User may wish to change the number of grid
#'  points for computational reasons.
#' @param ... Parameters to pass to `createSensitivityResults` or
#'  `createSensitivityResults_relativeMagnitudes`.
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  
  # Recover variance-covariance matrix ----
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Check time vector is consecutive with referencePeriod = -1
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if ( consecutivePre | consecutivePost ) {
    msg <- "honest_did expects a time vector with consecutive time periods;"
    msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
    stop(msg)
  }
  
  # Remove the coefficient normalized to zero
  hasReference <- any(es$egt == referencePeriod)
  if ( hasReference ) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex,-referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < referencePeriod))
  npost    <- nperiods - npre
  if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
    if ( npost <= 0 ) {
      msg <- "not enough post-periods"
    } else {
      msg <- "not enough pre-periods"
    }
    msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
    stop(msg)
  }
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}

# ------------------ #
## 7.2 Graph -----
# ------------------ #

calsan_did <- did::att_gt(
  yname = "cnae_dummy_v4",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)


es <- est_calsan


#Run sensitivity analysis for relative magnitudes
sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)


sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.05, to = 1, by = 0.1))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)


# --------------- #
## 7.3 Rais----
# --------------- #

calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)


es <- est_calsan

sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)



es_inf_func <- es$inf.function$dynamic.inf.func.e

n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n


pre_treatV <- V[1:8, 1:8, drop = FALSE]

# Assuming pre_treatV is your 8×8 matrix
M_row <- matrix(1/8, nrow = 1, ncol = 8)  # 1×8
M_col <- matrix(1/8, nrow = 8, ncol = 1)  # 8×1

# Multiply: (1×8) %*% (8×8) → 1×8, then %*% (8×1) → 1×1
result_1x1 <- M_row %*% pre_treatV %*% M_col


# ---------------------------------------------------------------------------- #
# 8. RAIS TWFE ----
# ---------------------------------------------------------------------------- #

#' Here we will test the TFWE in the main specification
colnames(data)


twfe <- feols(cnae_dummy_v4 ~ i(time_to_treat, ref = -1) | code_id + ano_sexo + ano_branco + ano_ensino,
              data = data,
              cluster = ~ code_id)

iplot(twfe)

#----------------------------------------------------------------------------- #
# <<< CBO >>> ----
# ---------------------------------------------------------------------------- #

#' In this section we will test how the relative probability of RAIS presence is
#' observed for different CBO groups.

# 10. CBO----
# Looking at RAIS within each CBO group
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cbo_group == 0 & ano < year_first_treated)[1],
    aux2 = which(cbo_group != 0 & ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup()



data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    cbo_first = cbo_group[row_number() == aux2]
)

## 10.1 Data Frames ----

#Note that WC = White-Collar and BC = Blue-Collar

### 10.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1 &
           cbo_first %in% c(1:5)) #Removing the individuals who don't start as WC

summary(data_wc$cbo_group)
summary(data_wc$cbo_first)

### 10.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0 &
           cbo_first %in% c(6:10)) #Removing the individuals who don't start as BC

summary(data_bc$cbo_group)
summary(data_bc$cbo_first)


## 10.2 Estimation ----
### 10.2.1 WC ----

data_final <- data.frame()

#WC
for ( wc in c(1:5)) {
  
  ini <- Sys.time()
  
  temp <- data_wc %>% 
    filter( cbo_first == wc)

  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
   print(est_calsan)
  # 
   plot_calsan <- ggdid(est_calsan) 

  # 
   plot_calsan
  
  #Result dataframe
   data_calsan <- ggplot_build(plot_calsan)$data[[1]]
   data_calsan$cbo <- wc
   data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
  fim <- Sys.time()
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  print("---------------------------------------------")
  print(paste0("Total time elapsed: ",mins," mins e ", secs, " s"))
  print("---------------------------------------------")
  rm(delta, ini, fim, mins, secs)
  
}

### 10.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cbo == 1 ~ x - 0.25,
      cbo == 2 ~ x - 0.15,
      cbo == 4 ~ x + 0.15,
      cbo == 5 ~ x + 0.25,
      TRUE ~ x
    )
  ) %>%
  filter(cbo != 1)

data_final$cbo <- as.factor(data_final$cbo)

p_wc <- ggplot(data_final, aes(x = x, y = y, color = cbo, shape = cbo, group = cbo)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c(
      #'black',
       '#0072B2', '#009E73'	, '#E69F00', "#CC79A7"),
    labels = c(
      #"Group 0",
               "Public Power, Directors and Managers", #G1
               "Sciences and Arts", #G2
               "Mid-level Technicians", #G3
               "Administrative Services Workers" #G4
               )
  ) +
  scale_shape_manual(
    values = c(
      #16,
      15, 17, 18, 16), # Circle, Square, Triangle, Diamond
    labels = c(
      #"Group 0",
      "Public Power, Directors and Managers", #G1
      "Sciences and Arts", #G2
      "Mid-level Technicians", #G3
      "Administrative Services Workers" #G4
    )
  ) +  
  labs(title = "White-collars",
    x = "Years to treatment", y = '', colour = '', shape = '') +
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
    
    legend.text = element_text(size = 11),
    legend.position = c(0.05, 0.05),         
    legend.justification = c(0, 0)           
  ) +
  scale_x_continuous(limits = c(-9.5, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p_wc


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/WC_groups.jpeg", plot = p_wc, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/WC_groups.pdf", plot = p_wc, device = "pdf", width = 10, height = 6, dpi = 300)

### 10.2.1 BC ----

data_final <- data.frame()

#BC
for ( wc in c(6,8,9,10)) {
  
  ini <- Sys.time()
  
  
  temp <- data_bc %>% 
    filter( cbo_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  #   #Result dataframe
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cbo <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
  fim <- Sys.time()
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  print("---------------------------------------------")
  print(paste0("Total time elapsed: ",mins," mins e ", secs, " s"))
  print("---------------------------------------------")
  rm(delta, ini, fim, mins, secs)
  
}

### 10.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cbo == 6 ~ x - 0.25,
      cbo == 7 ~ x - 0.15,
      cbo == 9 ~ x + 0.15,
      cbo == 10 ~ x + 0.25,
      TRUE ~ x
    )
  )

data_final$cbo <- as.factor(data_final$cbo)

p_bc <- ggplot(data_final, aes(x = x, y = y, color = cbo, shape = cbo, group = cbo)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c('black', '#0072B2', '#009E73'	, '#E69F00'
               #, "#CC79A7"
               ),
    labels = c("Service Workers, Shop Salespersons", #G5
               #"Group 7",
               "Production and Industrial Services", #G7
               "Production and Industrial Services", #G8
               "Repair and Maintance Services"   #G9
    )
  ) +
  scale_shape_manual(
    values = c(16, 15, 17, 18
               #, 16
               ), # Circle, square, triangle, diamond
    labels = c("Service Workers, Shop Salespersons", #G5
               #"Group 7",
               "Production and Industrial Services", #G7
               "Production and Industrial Services", #G8
               "Repair and Maintance Services"   #G9
    )
  ) +  
  labs(title = "Blue-collars",
    x = "Years to treatment", y = '', colour = '', shape = '') +
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
    
    legend.text = element_text(size = 11),
    legend.position = c(0.05, 0.05),         
    legend.justification = c(0, 0)           
  ) +
  scale_x_continuous(limits = c(-9.5, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p_bc


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/BC_groups.jpeg", plot = p_bc, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/BC_groups.pdf", plot = p_bc, device = "pdf", width = 10, height = 6, dpi = 300)
 

# ---------------------------------------------------------------------------- #
## 10.3 Combined plot ----
# ---------------------------------------------------------------------------- #

library(patchwork)

grid_plot <- ( p_wc + p_bc) #Combined plot

ggsave( #Saving image
  filename = paste0("cbo_combined_robust.png"),
  plot = grid_plot,
  path = "C:/Users/tuffy/Documents/IC/Graphs/united/",
  width = 1500/96, height = 620/96, dpi = 300
)



# ---------------------------------------------------------------------------- #
# <<< CNAE >>> ----
# ---------------------------------------------------------------------------- #

#'The same methodology is applied to this section. We seek to observe how each
#'CNAE group differs from the overall observed result in RAIS.

# 11. CNAE----
# Database
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cnae_group == 0 & ano < year_first_treated)[1], 
    aux2 = which(cnae_group != 0 & ano < year_first_treated)[1] # Removing NA values
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup() %>% 
  mutate(
    cnae_group = ifelse(cnae_group > 0, cnae_group - 1, cnae_group)
  )



data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    cnae_first = cnae_group[row_number() == aux2]
  )

## 11.1 Bases ----

### 11.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1) #Removing the individuals who don't start as WC

summary(data_wc$cnae_group)
summary(data_wc$cnae_first)

### 11.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0) #Removing the individuals who don't start as BC

summary(data_bc$cnae_group)
summary(data_bc$cnae_first)

# ---------------------------- #
## 11.2 WC ----
# ---------------------------- #
### 11.2.1 Estimation----

data_final <- data.frame()

#WC
for ( wc in c(1:18)) {
  
  
  temp <- data_wc %>% 
    filter( cnae_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cnae <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 11.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cnae == 1 ~ x - 0.30,
      cnae == 2 ~ x - 0.25,
      cnae == 3 ~ x - 0.20,
      cnae == 4 ~ x - 0.15,
      cnae == 5 ~ x - 0.10,
      cnae == 6 ~ x - 0.05,
      cnae == 7 ~ x - 0.03,
      cnae == 9 ~ x + 0.03,
      cnae == 10 ~ x + 0.05,
      cnae == 11 ~ x + 0.10,
      cnae == 12 ~ x + 0.15,
      cnae == 13 ~ x + 0.20,
      cnae == 14 ~ x + 0.25,
      cnae == 15 ~ x + 0.30,
      TRUE ~ x
    )
  )

data_final$cnae <- as.factor(data_final$cnae)


shapes18 <- c(0:17)   # squares, circles, triangles, etc.
colors18 <- RColorBrewer::brewer.pal(8, "Set2") |> 
  c(RColorBrewer::brewer.pal(12, "Paired")) |> 
  head(18)


p <- ggplot(data_final, aes(x = x, y = y, color = cnae, shape = cnae, group = cnae)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "right",  # move legend outside
    legend.box.margin = margin(0, 20, 0, 0) # add spacing from plot
  ) +
  scale_color_manual(values = colors18) +
  scale_shape_manual(values = shapes18) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = -9:4,
    labels = c(-9:-1, 0, "+1","+2","+3","+4")
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.1555),
    breaks = seq(-0.90, 0.15, 0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )
p


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_WC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_WC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------- #
## 11.3.1 BC ----
# ---------------------------- #

### 11.3.1 REG ----


#BC
for ( wc in c(1:7,9:16,18)) {
  message("Rodando para: ", wc," ...")
  
  if( wc == 1) {
    data_final <- data.frame()
  }
  
  temp <- data_bc %>% 
    filter( cnae_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cnae <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 11.3.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cnae == 6 ~ x - 0.25,
      cnae == 7 ~ x - 0.15,
      cnae == 9 ~ x + 0.15,
      cnae == 10 ~ x + 0.25,
      TRUE ~ x
    )
  )

data_final$cnae <- as.factor(data_final$cnae)

shapes18 <- c(0:17)   # squares, circles, triangles, etc.
colors18 <- RColorBrewer::brewer.pal(8, "Set2") |> 
  c(RColorBrewer::brewer.pal(12, "Paired")) |> 
  head(18)


p <- ggplot(data_final, aes(x = x, y = y, color = cnae, shape = cnae, group = cnae)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "right",  # move legend outside
    legend.box.margin = margin(0, 20, 0, 0) # add spacing from plot
  ) +
  scale_color_manual(values = colors18) +
  scale_shape_manual(values = shapes18) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = -9:4,
    labels = c(-9:-1, 0, "+1","+2","+3","+4")
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.1555),
    breaks = seq(-0.90, 0.15, 0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )
p



ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_BC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_BC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
# <<< SAL >>> ----
# ---------------------------------------------------------------------------- #

#' This is a WORK IN PROGRESS. I am tryng to better understang how each salary
#' quartile individual reacts with the labor lawsuit initiaon.
#' 
#' I must add that the current code dividing the groups in each years is badly
#' written. I have, still, to correct the issue regarding the quartile division 
#' for each year in the dataset.

## 12.1 Data ----
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(!is.na(remuneracao_media_sm_) &
                   ano < year_first_treated)[1],
    aux2 = which(remuneracao_media_sm_ != 0 & !is.na(remuneracao_media_sm_) &
                   ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup() 

data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    sm_first = remuneracao_media_sm_[row_number() == aux1],
    
    #For each year
    sm_03 = remuneracao_media_sm_[ano == 2003],
    sm_04 = remuneracao_media_sm_[ano == 2004],
    sm_05 = remuneracao_media_sm_[ano == 2005],
    sm_06 = remuneracao_media_sm_[ano == 2006],
    sm_07 = remuneracao_media_sm_[ano == 2007],
    sm_08 = remuneracao_media_sm_[ano == 2008],
    sm_09 = remuneracao_media_sm_[ano == 2009],
    sm_10 = remuneracao_media_sm_[ano == 2010],
    sm_11 = remuneracao_media_sm_[ano == 2011],
    sm_12 = remuneracao_media_sm_[ano == 2012],
    sm_13 = remuneracao_media_sm_[ano == 2013]
  )

##12.2 Variable ----
summary(data %>% filter(ano == 2008) %>% select(sm_first))
#' 1 - 1.520
#' 2 - 2.040
#' 3 - 3.160





### 12.2.1 Breaks ----
# ---------------------------------------------------------------------------- #
#' Before estimating the cutoff values I have to extract the quartile values for
#' each year in the database. If I do not calculate this value before hand, and
#' try to place it within the group_by fucntion, it will calculate the quartiles
#' individually and not for the entire data. Thus the primary extraction is needed.
# ---------------------------------------------------------------------------- #

break_first <- unique(quantile(data$sm_first, #Definindo os quartis
                               probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_03 <- unique(quantile(data$sm_03, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_04 <- unique(quantile(data$sm_04, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_05 <- unique(quantile(data$sm_05, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_06 <- unique(quantile(data$sm_06, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_07 <- unique(quantile(data$sm_07, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_08 <- unique(quantile(data$sm_08, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_09 <- unique(quantile(data$sm_09, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_10 <- unique(quantile(data$sm_10, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_11 <- unique(quantile(data$sm_11, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_12 <- unique(quantile(data$sm_12, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))
break_13 <- unique(quantile(data$sm_13, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))

ini <- Sys.time()


data <- data %>% 
  group_by(code_id) %>%
  mutate(
    quartile_first = cut(sm_first, #first observed salary value
                         breaks = break_first,
                             include.lowest = TRUE, labels = FALSE), 
        
        #Calculating for the years
    quartile_03 = cut(sm_03,
                          breaks = break_03,
                          include.lowest = TRUE, labels = FALSE),
    quartile_04 = cut(sm_04,
                     breaks = break_04,
                     include.lowest = TRUE, labels = FALSE), #2004
    quartile_05 = cut(sm_05,
                     breaks = break_05,
                     include.lowest = TRUE, labels = FALSE), #2005
    quartile_06 = cut(sm_06,
                     breaks = break_06,
                     include.lowest = TRUE, labels = FALSE), #2006
    quartile_07 = cut(sm_07,
                     breaks = break_07,
                     include.lowest = TRUE, labels = FALSE), #2007
    quartile_08 = cut(sm_08,
                     breaks = break_08,
                     include.lowest = TRUE, labels = FALSE), #2008
    quartile_09 = cut(sm_09,
                     breaks = break_09,
                     include.lowest = TRUE, labels = FALSE), #2009
    quartile_10 = cut(sm_10,
                     breaks = break_10,
                     include.lowest = TRUE, labels = FALSE), #2010
    quartile_11 = cut(sm_11,
                     breaks = break_11,
                     include.lowest = TRUE, labels = FALSE), #2011
    quartile_12 = cut(sm_12,
                     breaks = break_12,
                     include.lowest = TRUE, labels = FALSE), #2012
    quartile_13 = cut(sm_13,
                     breaks = break_13,
                     include.lowest = TRUE, labels = FALSE) #2013
    ) %>% 
  ungroup()


fim <- Sys.time()
delta <- difftime(fim, ini, units = "mins")
message(delta)



rm(break_03, break_04, break_05, break_06, break_07, break_08, break_09, break_10,
   break_11, break_12, break_13, break_first, fim, ini, delta)

##12.3 Graph ----

salary_list <- c("quartile_03", #2003 and so on
                 "quartile_04",
                 "quartile_05",
                 "quartile_06",
                 "quartile_07",
                 "quartile_08",
                 "quartile_09",
                 "quartile_10",
                 "quartile_11",
                 "quartile_12",
                 "quartile_13")

for (ano in c(2003:2013)) {
  
  gc()
  message("Starting: ", ano)
  
  #Timer start
  ini <- Sys.time()
  
  #Index capture
  i <- ano - 2002
  
  ext <- as.character(salary_list[i]) #extraction key
  
  
  ###12.3.1 Loop ----
  #Looping the regression for each quartile
  for( qrt in c(1,2,3,4)) {
      
      if(qrt == 1) {
        data_final <- data.frame()
      }
      
      
      temp <- data %>% 
        filter( get(ext) == qrt)
      
      
      #Estimation
      calsan_did <- did::att_gt(
        yname = "rais_",
        gname = "year_first_treated",
        idname = "code_id",
        tname = "ano",
        xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
        ,
        data = temp,
        control_group = "notyettreated",
        base_period = "universal",
        clustervars = "code_id"
      )
      
      est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
      #Result table
      print(est_calsan)
      # 
      plot_calsan <- ggdid(est_calsan) 
      
      # 
      plot_calsan
      # 
      #
      # Results dataframe 
      data_calsan <- ggplot_build(plot_calsan)$data[[1]]
      data_calsan$quart <- qrt
      data_final <- rbind(data_final,as.data.frame(data_calsan))
      
      rm(temp, calsan_did)
      
      message("Ended DrDiD estimation for quartile: ", qrt, " of ", ano)
      
}


    #### 12.3.1.1 Graph ----
    data_final <- data_final %>% 
      mutate(
        x = as.numeric(x),
        x = case_when(
          quart == 1 ~ x - 0.25,
          quart == 2 ~ x - 0.15,
          quart == 4 ~ x + 0.15,
          quart == 5 ~ x + 0.25,
          TRUE ~ x
        )
      ) 
    
    data_final$quart <- as.factor(data_final$quart)
    
    p <- ggplot(data_final, aes(x = x, y = y, color = quart, shape = quart, group = quart)) +
      geom_point( size = 3) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
      geom_hline(yintercept = 0, color = "#D62728") +
      geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
      scale_color_manual(
        values = c(
          #'black',
          '#0072B2', '#009E73'	, '#E69F00', "#CC79A7"),
        labels = c(
          #"Group 0",
          "1st Quartile",
          "2nd Quartile",
          "3rd Quartile",
          "4th Quartile"
        )
      ) +
      scale_shape_manual(
        values = c(
          #16,
          15, 17, 18, 16), # Circle, square, triangle, diamond
        labels = c(
          #"Group 0",
          "1st Quartile",
          "2nd Quartile",
          "3rd Quartile",
          "4th Quartile"
        )
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
      scale_x_continuous(limits = c(-9.5, 4.5),
                         breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                         labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
      scale_y_continuous(limits = c(-0.95, 0.155),
                         breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                         labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
    p
    
    ggsave(paste0("C:/Users/tuffy/Documents/IC/Graphs/salary/",ano,"_SM_quart.jpeg"), plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
    ggsave(paste0("C:/Users/tuffy/Documents/IC/Graphs/salary/",ano,"_SM_quart.pdf"), plot = p, device = "pdf", width = 10, height = 6, dpi = 300)
 
    
    #Final time
    fim <- Sys.time()
    
    delta <- difftime(fim, ini, units = "secs")
    mins <- floor(as.numeric(delta) / 60)
    secs <- round(as.numeric(delta) %% 60)
    
    message("---------------------------------------------")
    message("Total time: ",mins," mins and ", secs, " s")
    message("---------------------------------------------")
    
    rm(p, qrt, ano, ext, ini, fim, delta, secs, mins, i)   
}
