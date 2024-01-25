library(tidyverse)

master_data.df <- read_excel("Data/Kuwait Presentation Data.xlsx")

data2scores <- master_data.df %>%
  summarise(
    across(
      everything(),
      mean, 
      na.rm = T
      )
  ) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "values") %>%
  mutate(
    percentage = paste0(round(values*100,0),"%")
  ) %>%
  arrange(category)

writexl::write_xlsx(data2scores, path = "Output/general_scores.xlsx")
