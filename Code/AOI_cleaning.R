## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Data Validation-  TPS Threshold Comparison
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 24th, 2024
##
## This version:      January 24th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

AOI_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================

  matchaoi<- vmatch%>%
    filter(Source == "Arab Opinion Index")
  targetvars<- matchaoi$Question
  
  
  ## 1.2 Sub-setting data=======================================================================================
  
  df2<- df%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the appropriate vector to reorient 
  
  #Yes/No questions
  ro<- c("Q109")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 1, ifelse(oriented[[i]] == 2, 0, NA_real_))
  }
  
  ro2<- c("Q201_2", "Q2022_76_13")
  
  for(i in ro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, ifelse(oriented[[i]] == 3, 2, 
                          ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  
  ## 1.4 Normalize indicators ==================================================================================
  df3<- oriented
  
  df3[nrow(df3) + 1,] <- c(rep(list(1), ncol(df3)))
  df3[nrow(df3) + 1,] <- c(as.list(matchaoi$Scale))
  
  process<- preProcess(df3, method = c("range"))
  normalized <- predict(process, df3)
  
  normalized <- slice(normalized, 1:(n() - 2))
  
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  aggregate<- normalized%>%
    summarise_at(targetvars, mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2. Preparing Data                                                                       ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  return(aggregate)
  
}