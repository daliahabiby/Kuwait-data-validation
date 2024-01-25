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

####################################################
# Correspondence patterns
# 
# 1. time_changes = t-test function
# 2. tps_comparisson = TPS function
# 3. tps_trend_comparisson = TPS trends function
#
####################################################


# Define analysis functions

time_changes.df <- time_changes(data.df = master_data.df,
                                type    = "real",
                                country = args[1])

tps_comparisson.df <- TPS_function(country = args[1],
                                   gpp     = master_data.df,
                                   tps     = TPS.df,
                                   mat     = metadata
                                   )

#sociodem_comparisson.df <- sociodem_comparisson()

#missing_values.df<- missing_values(data= master_data.df, 
#                                   country= country)

# List of analysis functions

if(type_data == "pretest") {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df
    #sociodem_comparisson = sociodem_comparisson.df
    )
  
} else {
  
  analysis_functions <- list(
    time_changes = time_changes.df,
    tps_comparisson = tps_comparisson.df,
    tps_trend_comparisson = tps_trend_comparisson.df,
    sociodem_comparisson = sociodem_comparisson.df
  )
  
}

analysis.list <- analysis_functions


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Saving function                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Outcomes function                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




