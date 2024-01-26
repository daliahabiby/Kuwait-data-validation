## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Kuwait Data Validation-  TPS Threshold Comparison
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


TPS_function<- function(gpp, tps, country, mat){
  
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                1.  Data Wrangling                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
  ## 1.1 Identifying indicators    =============================================================================
  
  tpsvars<- mat$Question
  gppvars<- mat$GPP_Questionnaire

  
  ## 1.2 Sub-setting data  =====================================================================================
  gpp2<- gpp %>% 
    select(all_of(gppvars)) 

  ## 1.3 Re-orient indicators ==================================================================================
  
  ## 1.5 Aggregate indicators at the country level =============================================================
  
  gppaggregate <- gpp2%>%
    summarise_at(gppvars, mean, na.rm= TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ##                2.  Threshold Test                                                                      ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #exm$TPS_source<- ifelse(exm$TPS_source == "VDEM", "Varieties of Democracy", "Freedom in the World")
  
  aoindex<- AOI_clean(AOI)
  abarometer<- ABR_clean(ABR)
  
  tps<- cbind(aoindex, abarometer)
  
  final<- as.data.frame(matrix(nrow=0, ncol=8))
  colnames(final)<- c("Country", "GPP_Variable_Name", "GPP_datapoint", "TPS_Variable_Name", "TPS_datapoint", "TPS_Source", 
                       "Difference", "Flag")
 
  for (i in c(1:length(tpsvars))){
    
    t<- tps%>%
      select(tpsvars[[i]])
    
    g<- gppaggregate%>%
      select(gppvars[[i]])
    
    gp<- g[[1,1]]
    tp<- t[[1,1]]
    
    diff<- abs(gp - tp)
    
    f<- tibble("Country" = "Kuwait", "GPP_Variable_Name" = gppvars[[i]], "GPP_datapoint" = gp, 
               "TPS_Variable_Name" = tpsvars[[i]], 
               "TPS_datapoint" = tp, "TPS_Source" = mat$Source[[i]], 
               "Difference" = diff, "Flag" = ifelse(diff > .30, "red", ifelse(diff> .15, "yellow", "green")))
    
    final<- rbind(final, f)
  }
  
  return(final)
  
}
