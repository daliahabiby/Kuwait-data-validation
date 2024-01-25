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

ABR_clean<- function(df){
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## 1.1 Identifying indicators  ===============================================================================
  
  matchabr<- vmatch%>%
    filter(Source == "Arab Barometer")
  targetvars<- matchabr$Question
  
  
  ## 1.2 Sub-setting data=======================================================================================
  
  df2<- df%>%
    select(all_of(targetvars))
  
  ## 1.3 Re-orient indicators ==================================================================================
  
  oriented<- df2
  
  # Check the codebook to see which variables need to be reoriented. Add them in the appropriate vector to reorient 
  
  #Yes/No questions
  ro<- c("Q521_2A", "Q521_6")
  
  for(i in ro){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 4, ifelse(oriented[[i]] == 2, 3, ifelse(oriented[[i]] == 3, 2, 
                                                                                       ifelse(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  ro2<- c("Q210")
  
  for (i in ro2){
    oriented[[i]]<- ifelse(oriented[[i]] %in% c(1:4), oriented[[i]], NA_real_)
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  df3<- oriented
  
  df3[nrow(df3) + 1,] <- c(rep(list(1), ncol(df3)))
  df3[nrow(df3) + 1,] <- c(as.list(matchabr$Scale))
  
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
  
  aggregate$country<- "Kuwait"
  
  return(aggregate)
  
}