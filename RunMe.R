## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           Kuwait Data Validation
##
## Script:            Data validation
##
## Author(s):         Dalia Habiby        (Dhabiby@worldjusticeproject.org)
##                    Santiago Pardo G.   (spardo@worldjusticeproject.org)         
##
## Dependencies:      World Justice Project
##
## Creation date:     January 19th, 2024
##
## This version:      January 19th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("Code/settings.R")
source("Code/AOI_cleaning.R")
source("Code/ABR_cleaning.R")
source("Code/ttest.R")


master_data.df <- read_dta(paste0(path2SP, 
                        "/Presentations/Kuwait/Kuwait-data-validation/Inputs/Kuwait_2023.dta" 
                       ))
aoi<- read_dta(paste0(path2SP, 
                        "/Presentations/Kuwait/Kuwait-data-validation/Inputs/Arab Opinion Index 2022.dta"
                       ))
AOI<- aoi%>%
  filter(Q1 == 8)
abar<- read_dta(paste0(path2SP, 
                        "/Presentations/Kuwait/Kuwait-data-validation/Inputs/AB7_ENG_Release_Version6.dta"
                       ))
ABR<- abar%>%
  filter(COUNTRY == 9)

var_matches<- read_xlsx(paste0(path2SP, 
                              "/Presentations/Kuwait/Kuwait-data-validation/Inputs/matches_kuwait.xlsx"
))
vmatch<- var_matches%>%
  filter(!is.na(GPP_Questionnaire))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Defining analysis Functions                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Define analysis functions

source("Code/data_prep.R")

full       <-  variables%>%
                        pivot_longer(cols=all_of(vars_names), names_to = "Variable")%>%
                        group_by(Variable)%>%
                        summarise("Mean_Value" = mean(value, na.rm=TRUE))

nation.test <- ttest.fn(data_set.df = data_subset.df,
                        dependent_vars = vars_names,
                        independent_var = "nation",
                        grupo1 = "Native",
                        grupo2 = "Foreign")
gender.test <- ttest.fn(data_set.df = data_subset.df,
                        dependent_vars = vars_names,
                        independent_var = "gend",
                        grupo1 = "Male",
                        grupo2 = "Female")
financial.test <- ttest.fn(data_set.df = data_subset.df,
                        dependent_vars = vars_names,
                        independent_var = "fin",
                        grupo1 = "Financially Secure",
                        grupo2 = "Financially Insecure")



# List of analysis functions
  
  analysis_functions <- list(
    overall = full,
    nationality = nation.test,
    gender = gender.test,
    financial = financial.test
    #sociodem_comparisson = sociodem_comparisson.df
    )
  


analysis.list <- analysis_functions


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

openxlsx::write.xlsx(analysis.list,
                     paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Kuwait.xlsx"
                           ))

