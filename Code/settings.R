## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S Data Validation
##
## Script:            Pre-settings
##
## Author(s):         Carlos A. ToruC1o Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Dalia Habiby                (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 23rd, 2024
##
## This version:      January 23rd, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

p_load(char = c(
  
  # Data Loading
  "haven", "readxl", "writexl", "haven", "rio",
  
  # Utilities
  "caret",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "ctoruno") {
  path2SP <- paste0("/Users/ctoruno/OneDrive - World Justice Project/Data Analytics")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics")
  
} else if (Sys.info()["user"]=="Dhabiby"){
  path2SP<- paste0("/Users/Dhabiby/World Justice Project/Research - Data Analytics")
  
} else if (Sys.info()["user"] == "apillai") {
  path2SP <-"/Users/apillai/OneDrive - World Justice Project/Data Analytics"

}else {
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Re-orienting, normalizing, aggregating                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

normalizingvars<- function(gppctry, gppvars){

  oriented<- gppctry
  
  for(i in gppvars){
    
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(98,99), NA_real_, oriented[[i]])
    
  }
  
  ro<- c("JSE_indjudges", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", "CPA_cleanelec_local", 
         "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "LEP_bribesreq", "IRE_campaign",
         "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "TRT_parliament", "TRT_police", 
         "TRT_pparties", "CTZ_laborcond", "JSE_equality", "CJP_proofburden", "JSE_rightsaware", "JSE_access2assis", "JSE_affordcosts", "JSE_quickresol", "COR_judges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "CPB_community", "CPB_freeassoc", "COR_govt_local", "COR_parliament", "ROL_equality_sig", "JSE_polinfluence", "COR_govt_national", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CPA_law_langaval", "CPB_unions", "CPB_freemedia", "CPA_partdem_congress", "CPB_freexp_pp","CPA_partdem_localgvt")
  
  ro2<- c("CPA_protest", "CPA_cso")
  
  gppro<- intersect(gppvars, ro)
  gppro2<- intersect(gppvars, ro2)
  
  for(i in gppro){
  
      oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, 
                                                       ifelse(oriented[[i]] == 3, 2, ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }

  for(i in gppro2){
  
      oriented[[i]]<- ifelse(oriented[[i]] == 1, 2, ifelse(oriented[[i]] == 2, 1, NA_real_))
    }

## 1.4 Normalize indicators ==================================================================================
cols_oriented <- names(oriented)[2:length(oriented)]
max_values <- lapply(cols_oriented, function(col_name){
  
  codebook.df %>% 
    filter(Variable %in% col_name) %>%
    mutate(max_value = 
             case_when(
               Scale == "Scale 2" ~ 2,
               Scale == "Scale 3" ~ 3,
               Scale == "Scale 4" ~ 4,
               Scale == "Scale 5" ~ 5
             )) %>%
    pull(max_value)
})

oriented[nrow(oriented) + 1,] <- c("maxs", max_values)
oriented[nrow(oriented) + 1,] <- c("mins", rep(list(1), ncol(oriented)-1))


process    <- preProcess(oriented, method = c("range"))
normalized <- predict(process, oriented)

normalized <- slice(normalized, 1:(n() - 2))

return(normalized)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data list                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gpp_vars<-c("JSE_indjudges", "ROL_courtrulings_imp", "ORC_govtefforts", "ORC_impartial_measures", "CPA_freevote", 
           "CPA_cleanelec_local", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "PAB_emergpower",
           "PAB_overcourts", "PAB_manipulelect", "PAB_attackmedia", "PAB_misinfo", "ROL_corruption_imp", "LEP_bribesreq",
           "ORC_corimpact", "COR_3year_change", "BRB_health_B", "BRB_permit_B", "ROL_abusepower_imp", "ORC_pconnections",
           "IRE_campaign", "IPR_easy2read", "IPR_rights", "IPR_easy2find", "IPR_easy2find_online", "CPA_media_freeop",
           "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "CPA_protest", "CPA_cso", "TRT_parliament", "TRT_police",
           "TRT_pparties", "CPA_media_freeop", "CPB_freexp_cso", "CPA_freepolassoc", "CPB_freexp", "ROL_equality_imp",
           "CTZ_laborcond_A", "CTZ_laborcond_A", "CPA_freevote", "JSE_equality", "ROL_constprotection_imp",
           "CJP_proofburden", "SEC_orgcrime", "JSE_rightsaware", "JSE_access2assis", "JSE_access2assis", "JSE_affordcosts",
           "JSE_quickresol", "JSE_indjudges", "JSE_enforce", "LEP_indpolinv", "COR_police", "LEP_indprosecutors", 
           "COR_judges", "JSE_indjudges", "CJP_resprights", "CJP_fairtrial", "CJP_saferights", "COR_parliament", "COR_govt_national",
           "COR_govt_local", "COR_police", "IRE_govtbudget", "IRE_govtcontracts", "IRE_disclosure", "SEC_walking", "CTZ_accountability",
           "CPB_freeassoc", "CPA_law_langaval", "CPB_unions", "CPB_freexp", "CPA_cleanelec_local", "CPB_community", "CPB_freemedia",
           "CPB_freexp_cso", "CPA_partdem_congress", "CPB_freexp_pp", "CPA_partdem_localgvt", "LEP_rightsresp", "LEP_accountability"
           )

vars<- c("q46c_G2", "q46f_G2","q46g_G2","q46c_G1","q46e_G2","q46d_G2","q46f_G1","q46a_G2","q46d_G1","q46e_G1","q46h_G2",
         "q2a","q2d","q2b","q2c","q2e","q2f","q2g","q1a","q1d","q1b","q1c","q1e","q1f","q1g", "q43_G2", "q9", "q49a" , 
         "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "q49c_G1", "q49b_G1", "q48f_G2", "q48h_G1", "q48g_G2", 
         "q48c_G2", "q48b_G2", "q48a_G2", "q48b_G1", "q48a_G1", "q48c_G1", "q48d_G2", "q18a", "q18c", 
         "q18d", "q18e", "q48e_G2", "q48d_G1", "q50", "q51", "q45a_G1", "q45b_G1" , "q45c_G1", 
         "q4a", "q4b", "q4c", "q4d", "q4e", "q8d", "q8f", "q17_1", "q17_2","q17_3","q17_4","q17_5","q17_6","q17_7",
         "q17_8","q17_9", "q17_10","q17_11", "q17_12","q17_13","q17_14","q17_15", "q16a" , "q16b", "q16c", "q16d", "q16e"
         )

vars2<- paste0(vars, "_norm")

newnames<- c("Fundamental Freedoms - People can express opinion against the government", 
             "Fundamental Freedoms - Civil society organizations can express opinions against the government", 
             "Fundamental Freedoms - Political parties can express opinions against the government", 
             "Fundamental Freedoms - The media can express opinions against the government without fear of retaliation",
             "Fundamental Freedoms - The media can expose cases of corruption", 
             "Fundamental Freedoms - People can attend community meetings", 
             "Fundamental Freedoms - People can join any political organizations", 
             "Fundamental Freedoms - People can organize around an issue or petition", 
             "Fundamental Freedoms - Local government officials are elected through a clean process", 
             "Fundamental Freedoms - People can vote freely without feeling harassed or pressured", 
             "Fundamental Freedoms - Religious minorities can observe their holy days", 
             "Corruption - Members of the national assembly", 
             "Corruption - Police officers", 
             "Corruption - Local government officers", 
             "Corruption - National government officers", 
             "Corruption - Prosecutors", 
             "Corruption - Judges & magistrates", 
             "Corruption - Public Defense Attorneys", 
             "Trust - People living in their country", 
             "Trust - Police officers", 
             "Trust - Local government officers", 
             "Trust - National government officers", 
             "Trust - Prosecutors", 
             "Trust - Judges & magistrates", 
             "Trust - Public Defense Attorneys", 
             "Accountability - Government officials would be held accountable for breaking the law", 
             "Security - Perception of security: feeling safe walking in their neighborhood at night",
             "CJ - The criminal justice system is effective in bringing people who commit crimes to justice",
             "CJ - The criminal justice system allows all victims of crime to seek justice regardless of who they are",
             "CJ - The criminal justice system treats those accused of crime as 'innocent until proven guilty'", 
             "CJ - The criminal justice system allows all those accused of crimes to get a fair trial regardless of who they are",
             "CJ - The criminal justice system gives punishments which fit the crime", 
             "CJ - The criminal justice system makes sure everyone has access to the justice system if they need it",
             "CJ - The criminal justice system deals with cases promptly and efficiently",
             "Institutional Perfomance - Prosecutors prosecute crimes committed in an independent manner",
             "Institutional Perfomance - The public defenders do everything they can to defend poor people that are accused of committing a crime",
             "Institutional Perfomance - The judges decide cases in an independent manner and are not subject to any sort of pressure",
             "Police - Are available to help when needed", 
             "Police - Help them feel safe", 
             "Police - Resolve security problems in  the community", 
             "Police - Perform effective and lawful investigations", 
             "Police - Act lawfully", 
             "Police - Respect the rights of suspects", 
             "Police - Treat all people with respect", 
             "Police - Economic status", 
             "Police - Ethnic background", 
             "Police - Religion", 
             "Police - Foreigner status", 
             "Police - Investigate crimes in an independent manner", 
             "Police - Are held accountable for violating laws", 
             "Authoritarianism - Government efficiency is more important than citizen influence", 
             "Authoritarianism - The president should not be bound by the laws or courts", 
             "Contrainst - Congress could hypothetically stop a Head of State'sillegal actions", 
             "Contrainst - Courts could hypothetically stop a Head of State's illegal actions", 
             "Contrainst - Citizens could hypothetically stop a Head of State's illegal actions", 
             "Bribery - Request a government permit", 
             "Bribery - Request public benefits", 
             "Bribery - Obtain a bith certificate", 
             "Bribery - Secure a place at a public school", 
             "Bribery - Use a public health service", 
             "Security - Victims that reported crimes", 
             "Security - Cases ended in prosecution", 
             "Discrimination - Ancestry or national origin", 
             "Discrimination - Gender", 
             "Discrimination - Race", 
             "Discrimination - Age", 
             "Discrimination - Religion", 
             "Discrimination - Height", 
             "Discrimination - Weight", 
             "Discrimination - Physical appearence", 
             "Discrimination - Physical or mental disability",
             "Discrimination - Sexual orientation", 
             "Discrimination - Education or income level", 
             "Discrimination - Nationality or immigration status", 
             "Discrimination - Shade of skin color",
             "Discrimination - Tribe", 
             "Discrimination - Clothing or hairstyle", 
             "Discrimination - You are treated with less courtesy or respect than other people",
             "Discrimination - You receive poorer service than other people at restaurants or stores", 
             "Discrimination - People act as if they think you are not smart", 
             "Discrimination - People act as if they are afraid of you", 
             "Discrimination - You are threatened or harassed")


kuw<- master_data.df%>%
  select(all_of(vars2))

kuw<- kuw %>% rename_at(vars(unique(vars2)), ~ unique(newnames))


kuw <- kuw[,colSums(is.na(kuw))< (nrow(kuw)-30)]

writexl::write_xlsx(kuw, "Kuwait Presentation Data.xlsx")
