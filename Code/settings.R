## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           Kuwait Data Validation
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
  
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggrepel", "ggplotify", "gridExtra", "ggradar2", "patchwork", 
  "waffle", "ggh4x",
  
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


gppvars<- c("q46c_G2", "q46f_G2","q46g_G2","q46c_G1","q46e_G2","q46d_G2","q46f_G1","q46a_G2","q46d_G1","q46e_G1","q46h_G2",
         "q2a","q2d","q2b","q2c","q2e","q2f","q2g","q1a","q1d","q1b","q1c","q1e","q1f","q1g", "q43_G2", "q9", "q49a" , 
         "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "q49c_G1", "q49b_G1", "q48f_G2", "q48h_G1", "q48g_G2", 
         "q48c_G2", "q48b_G2", "q48a_G2", "q48b_G1", "q48a_G1", "q48c_G1", "q48d_G2", "q18a", "q18c", 
         "q18d", "q18e", "q48e_G2", "q48d_G1", "q50", "q51", "q45a_G1", "q45b_G1" , "q45c_G1", 
         "q4a", "q4b", "q4c", "q4d", "q4e", "q8d", "q8f", "q17_1", "q17_2","q17_3","q17_4","q17_5","q17_6","q17_7",
         "q17_8","q17_9", "q17_10","q17_11", "q17_12","q17_13","q17_14","q17_15", "q16a" , "q16b", "q16c", "q16d", "q16e", 
         "q7a", "q7b", "q7c", "q5", "q6a", "q6b", "q6c"
         )

gppvars2<- paste0(gppvars, "_norm")

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
             "Discrimination - You are threatened or harassed",
             "Open Government - Detailed budget figures of government agencies", 
             "Open Government - Copies of government contracts",
             "Open Government - Disclosure records of senior government officials (such as financial records or property holdings)",
             "Open Government - Have you made a request for information held and distributed by a government agency (such as government ministries, baladiya offices, law enforcement agencies, etc.) in the last 12 months?  ",
             "Open Government - Did you receive the information that you requested?",
             "Open Government - Approximately how long did it take to receive the information that you requested?",
             "Open Government - Did you have to pay any additional amount above the official cost or give anything of value in order to obtain the information?")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts

path2fonts<- paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Fonts/")
font_add(family     = "Lato Full",
         regular    = paste0(path2fonts, "Lato-Regular.ttf"),
         italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
         bold       = paste0(path2fonts, "Lato-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
font_add(family  = "Lato Light",
         regular = paste0(path2fonts, "Lato-Light.ttf"))
font_add(family  = "Lato Black",
         regular = paste0(path2fonts, "Lato-Black.ttf"))
font_add(family  = "Lato Black Italic",
         regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
font_add(family  = "Lato Medium",
         regular = paste0(path2fonts, "Lato-Medium.ttf"))
showtext_auto()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  Color Palette                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mainCOLOR      <- c("#2a2a9A")
countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f")
binPalette     <- c("#003b8a", "#fa4d57")
barsPalette    <- c("#2a2a9A", "#E2E2F7")
glinesPalette  <- c("#2a2a94", "#a90099", "#3273ff")
rosePalette    <- c("#20204a", "#12006b", "#2e2e95", "#4e43dd", "#756ef9", "#9c94ff", "#b1a6ff",
                    "#cfb3ff", "#e2a4ff", "#f2aadc", "#ffd7f5")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Creating a saving function                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveIT.fn <- function(chart, n, suffix = NULL, w, h) {
  ggsave(plot   = chart,
         file   = file.path("Outputs", 
                            str_replace_all(mainCountry, " ", "_"),
                            paste0("imgChart", n),
                            paste0("figure_", n, suffix, ".svg"),
                            fsep = "/"), 
         width  = w, 
         height = h,
         units  = "mm",
         dpi    = 72,
         device = "svg")
} 
