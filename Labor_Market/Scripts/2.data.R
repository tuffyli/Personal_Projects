# ---------------------------------------------------------------------------- #
# Data Organization
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 30/01/2025
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


# ---------------------------------------------------------------------------- #
# 1. First data construction -----
# ---------------------------------------------------------------------------- #

# base <- read.csv("C:/Users/tuffy.issa/Documents/IC/Bases/panel_workers_2003to2019.csv")


base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2019.csv")

base <- base %>%
  filter (ano < 2014)

## 1.1 Dummy for RAIS presence ####
base <- base %>%
  group_by(code_id) %>%
  mutate(
    rais_aux = ifelse(year_first_treated - ano == 1, #Dummy pre-treat
                      rais_, NA),
    max_aux = max(rais_aux, na.rm = TRUE),

    #Dummy for presence in RAIS before-treatment
    rais_aux2 = ifelse(any(rais_ == 1 & ano < year_first_treated), 1, 0),
    max_aux2 = max(rais_aux2, na.rm = TRUE)
  )


temp <- base

#Filtering
base <- base %>%
  filter( max_aux2 == 1)

#Sample
print(nrow(base)/nrow(temp))

##1.2 Fixed Effects and controls ----
base$ano_mun <- as.numeric(interaction(base$ano, base$cod_municipio))
                           
base <- base %>%
  select(
    -rais_aux,
    -max_aux
  )

##1.3 Arraning Control data ####
# Searching for control variables in the year before treat closest to treatment
base <- base %>%
  group_by(code_id) %>%
  mutate(
    raca_aux = ifelse(
      is.na(raca_initial),
      raca_[ano == max(ano[!is.na(raca_) & ano < year_first_treated], na.rm = TRUE)],
      raca_initial),

    sexo_aux = ifelse(
      is.na(sexo_initial),
      sexo_[ano == max(ano[!is.na(sexo_) & ano < year_first_treated], na.rm = TRUE)],
      sexo_initial),

    ensino_aux = ifelse(
      is.na(sexo_initial),
      escolaridade_[ano == max(ano[!is.na(escolaridade_) & ano < year_first_treated], na.rm = TRUE)],
      escolaridade_initial)
    )


#Creating dummies
base <- base %>%
  mutate(
    branco_dummy = ifelse( #Dummy raça/cor - race/color
      !is.na(raca_aux) & raca_aux == 2, 1, 0
    ),
    sexo_dummy = ifelse( #Dummy sex Male = 1
      !is.na(sexo_aux) & sexo_aux == 1, 1, 0
    ),

    ensino_dummy = ifelse(
      !is.na(ensino_aux) & ensino_aux >= 9, 1, 0
    )

  )
base$ano_sexo <- as.numeric(interaction(base$sexo_dummy, base$ano))
base$ano_branco <- as.numeric(interaction(base$branco_dummy, base$ano))
base$ano_ensino <- as.numeric(interaction(base$ensino_dummy, base$ano))


# ---------------------------------------------------------------------------- #
##1.4 CNAE CBO code stablishment ####
# ---------------------------------------------------------------------------- #

#Cnae numeric code
base$cnae_group <- as.numeric(factor(base$gcnae))
base$cnae_group <- ifelse(base$cnae_group == 1, 0, base$cnae_group)

#CBO numeric code
base$cbo_group <- as.numeric(cut( base$ocupacao_cbo2002_,
                                  breaks = c(-Inf,100000,200000, 300000, 400000, 500000, 600000, 700000,800000, 900000, Inf)))
str(base)

summary(base$cbo_group)


#Dealing with NA values in the CBO
base$cbo_group <- ifelse(is.na(base$cbo_group), 0, base$cbo_group)

## 1.5 Saving edited database ----
write.csv(base, "C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")



# ---------------------------------------------------------------------------- #
#2.  Rais presence ----
##2.1 More adjustments ----
# ---------------------------------------------------------------------------- #
base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")

#Defining the cohort of individuals observed in RAIS across all periods.
base <- base %>%   
  group_by(code_id) %>% 
  mutate( all_in_rais = min(rais_))

#Filtering variables
base <- base %>% 
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
  )


###2.1.1 Workers Group ----
#Separating the workers groups


strings <- unique(base$ocupacao_cbo2002_)

#Extracting the CBO strings
classification_collar <- data.frame(strings) %>%
  mutate(
    first_two_digits = substr(strings, 1, 2),
    first_digit = substr(strings, 1, 1),
    white_collar = case_when(
      first_two_digits %in% c("62", "63", "64", "34", "37", "51", "61" ) |
        first_digit %in% c("7", "8", "9") ~ 0, #Blue Collar
      
      
      first_two_digits %in% c("30", "31", "32", "33", "35", "39", "52") |
        first_digit %in% c("1", "2", "4") ~ 1, #White collar
      TRUE ~ NA_real_
    )
  ) %>% 
  select(strings, white_collar)
base <- merge(base, classification_collar,
              by.x = "ocupacao_cbo2002_", by.y = "strings", all.x = TRUE)


### 2.1.2 Salary auxiliar variable
#Creation of auxiliary (helper) and remuneration variables
base <- base %>% 
  mutate(time_to_treat = (ano - year_first_treated),
         treat = 1,
         previous_year = year_first_treated - 1,
         nvl_rem = log(remuneracao_media_sm_ + 1)
         ) %>%
  #Removing the CBO category values that are no longer needed
  select(-ocupacao_cbo2002_)



#Extracting the pre-treatment values
pre_treatment <- base %>%
  filter(ano == previous_year) %>%
  rename(
    cbo_pre_treat = cbo_group,
    rem_pre_treat = nvl_rem,
    mun_pre_treat = cod_mun_rais_,
    cnae_pre_treat = cnae_group
         ) %>% 
  select(
    code_id,
    previous_year,
    cbo_pre_treat,
    rem_pre_treat,
    mun_pre_treat,
    cnae_pre_treat
  )

# Merging with the original dataframe
base <- base %>%
  left_join(pre_treatment, by = c("code_id", "previous_year")) %>% 
  arrange(code_id)



# ------------------- #
##2.2 CNAE Dummy ----
# ------------------- #


summary(base$cnae_group)

# Steps for creating the dummy
treat <- base %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_
    ) %>%
  group_by(code_id) %>% 
  
  mutate(
    # Auxiliary dummy — free to change after treatment
    dummy_cnae_aux =        
      case_when(
        # Here we list the possible cases
          ano < year_first_treated & cnae_group == 0 ~ 1, 
          
          ano >= year_first_treated & cnae_group == 0 ~ 1,
          
          ano >= year_first_treated & cnae_group != 0 & !(cnae_group %in% cnae_group[ano < year_first_treated]) ~ 1,
          
          TRUE ~ 0
        ),
    
    #Final Dummy
    first_one = which(dummy_cnae_aux == 1 & ano >= year_first_treated)[1], #First post-treatment row
    
    dummy_cnae = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, dummy_cnae_aux
    ),

    dummy_cnae = ifelse(
      is.na(first_one) & ano >= year_first_treated, 0 , dummy_cnae #This condition handles cases where there is no first_one
    )
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae # selecting only the columns needed to perform the merge
  )

#joining the dataframes
base <- base %>% 
  left_join(treat, by= c("code_id", "ano"))

# ------------------ #
## 2.3 CBO Dummy ----
# ------------------ #

#information that matters for the CBO
treat <- base %>%
  select(
    code_id,
    ano,
    cbo_group,
    cbo_pre_treat,
    year_first_treated,
    rais_
  ) 


treat <- treat %>% 
  arrange(code_id, ano) %>% 
  group_by(code_id) %>% 
  mutate(
    dummy_cbo_aux =
      case_when(
        ano < year_first_treated & cbo_group == 0 ~ 1, 
        
        ano >= year_first_treated & cbo_group == 0 ~ 1,
        
        ano >= year_first_treated & cbo_group != 0 & !(cbo_group %in% cbo_group[ano < year_first_treated]) ~ 1,

        TRUE ~ 0
      )
  )

#The last dummy
treat <- treat %>% 
  group_by(code_id) %>% 
  mutate(
    first_one = which(dummy_cbo_aux == 1 & ano >= year_first_treated)[1], #First post-treatment row with change
    
    #Final Dummy
    dummy_cbo = case_when(
      ano < year_first_treated ~ dummy_cbo_aux,
      ano >= year_first_treated & row_number() >= first_one & !is.na(first_one) ~ 1,
      ano >= year_first_treated & is.na(first_one) ~ 0,
      TRUE ~ 0
    )
    
  )


treat <- treat %>% 
  select(
    code_id, ano, dummy_cbo
  )


base <- base %>%
  left_join(treat, by = c("code_id","ano")
            ) %>% 
  arrange(code_id, ano) 

# --------------------------------------------------------------------------- #
## 2.4 Mun -----
# --------------------------------------------------------------------------- #

#Here I will compare the first non-missing value with the remaining obs in the data
base <- base %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(is.na(cod_mun_rais_) & ano < year_first_treated)[1],
    aux2 = which(!is.na(cod_mun_rais_) & ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup()



base <- base %>%
  group_by(code_id) %>% 
  
  mutate(
    mun_first = cod_mun_rais_[row_number() == aux2],
    
    simple_mun_dummy = ifelse(!is.na(cod_mun_rais_) & cod_mun_rais_ != mun_first, 
                              1, 0)
  ) 


  base <- base %>% ungroup() %>% 
  select(-mun_first, -aux1, -aux2)

# --------------- #
##2.5 Collars ----
# --------------- #
#Separating individuals by collar type
base <- base %>% 
  group_by(code_id) %>% 
  mutate(
    white_dummy =
      case_when( 
        ano < year_first_treated & any(white_collar == 1) ~ 1,
        ano < year_first_treated & any(white_collar == 0) ~ 0,
        TRUE ~ NA_real_
      )
  )

#Largest value extraction
base <- base %>% 
  group_by(code_id) %>% 
  mutate(
    white_dummy = ifelse(any(!is.na(white_dummy)),
                        max(white_dummy, na.rm = T),
                        NA)
  )


#For variable summaries, stargazer requires numeric values
# Convert variables to numeric if needed (e.g., if they are factors)
base$sexo_dummy <- as.numeric(base$sexo_dummy)
base$branco_dummy <- as.numeric(base$branco_dummy)
base$ensino_dummy <- as.numeric(base$ensino_dummy)
base$white_dummy <- as.numeric(base$white_dummy)

# ---------------------------------------------------------------------------- #
## 2.6 Descriptive Statistics ----
# ---------------------------------------------------------------------------- #
stargazer(base[, c("sexo_dummy", "branco_dummy", "ensino_dummy", "white_dummy")], 
          type = "text",          
          title = "Descrição das variáveis",
          summary.stat = c("mean", "sd", "min", "max", "median"), 
          digits = 3,
          covariate.labels = c(
            "Sexo masculino",
            "Cor/raça branca",
            "Ensino Superior",
            "White Collar"
          ))



stargazer::stargazer(base,
                     type = "text",
                     digits = 3)


summary(base)

#Saving final
write.csv(base, "C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

