## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Kuwait Data Validation-  TPS Threshold Comparison
##
## Author:            Dalia Habiby   (dhabiby@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 25th, 2024
##
## This version:      January 25th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

kuw<- master_data.df%>%
  select(gend, nation, fin, all_of(gppvars2))

kuw<- kuw %>% rename_at(vars(unique(gppvars2)), ~ unique(newnames))


kuw <- kuw[,colSums(is.na(kuw))< (nrow(kuw)-30)]

#writexl::write_xlsx(kuw, "Kuwait Presentation Data.xlsx")


data_subset.df<- kuw%>%
  mutate(
  gend     =
    case_when(
      gend == 1 ~ "Male",
      gend == 2 ~ "Female"
    ),
  nation     =
    case_when(
      nation == 1 ~ "Native",
      nation == 2 ~ "Foreign"
    ),
  fin = if_else(fin == 1 | fin == 2 | fin == 3, "Financially Insecure",
              ifelse(fin == 4 | fin == 5, "Financially Secure", NA_character_))
  )

variables <- data_subset.df %>%
  select(!gend) %>%
  select(!nation) %>%
  select(!fin)

vars_names <- names(variables)
