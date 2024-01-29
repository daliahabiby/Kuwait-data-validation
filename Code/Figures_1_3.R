## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Kuwait Data Figures 1-3
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Dalia Habiby                (dhabiby@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 26th, 2024
##
## This version:      January 26th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Figure 1                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure01_01.fn <- function(nchart = 1, PAR = F) {
  
  # Defining variables to use

    vars4plot <- c("q50")
  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    select(country, all_of(vars4plot)) %>%
    mutate(
      across(!country,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(country) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(country, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(country, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(country, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("Positive", "Negative", "Neutral")
  
  
  
  data2plot$country<- ifelse(data2plot$country == "Kuwait", "Overall", NA_character_)
  ###########
  
  data2plot2 <- master_data.df %>%
    select(nation, all_of(vars4plot)) %>%
    mutate(
      across(!nation,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(nation) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot2 <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot2 %>%
                           select(nation, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(nation, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(nation, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  

  data2plot2$nation<- ifelse(data2plot2$nation == 1, "Kuwaiti", "Foreign")
  data2plot2<- rename(data2plot2, c("country" = "nation"))
  
  data2plot3<- rbind(data2plot, data2plot2)
  
  data2plot3$status<- ifelse(data2plot3$status == "Positive", "It is more important \nfor citizens to be able \nto hold government accountable, \neven if that means it makes \ndecisions more slowly.", ifelse(data2plot3$status == "Negative", "It is more important \nto have a government \nthat can get things done, \neven if we have no influence \nover what it does.", "Agree with neither \nstatement."))
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("It is more important \nto have a government \nthat can get things done, \neven if we have no influence \nover what it does.", "It is more important \nfor citizens to be able \nto hold government accountable, \neven if that means it makes \ndecisions more slowly.", "Agree with neither \nstatement.")
  
  # Applying plotting function
  chart <- divbars(data           = data2plot3,
                   target_var     = "perc",
                   grouping_var   = "country",
                   diverging_var  = "status",
                   negative_value = "Negative",
                   colors         = colors4plot,
                   labels     = "label",
                   lab_pos =  "lab_pos",
                   title = "Open Government: Information Requests",
                   subtitle = "Percentage of people who believe it is likely or very likely \nto recieve government information upon request",
                   legend = "Requested government \ninformation in the \nlast 12 months",
                   categories_grouping_var = c( "It is more important \nfor citizens to be able \nto hold government accountable, \neven if that means it makes \ndecisions more slowly.", "It is more important \nto have a government \nthat can get things done, \neven if we have no influence \nover what it does.","Agree with neither \nstatement.")
  )
  
  
  
  
  data2plot3<- rbind(data2plot, data2plot2)
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("Statement 1", "Statement 2", "Neither")
  
  data2plot3$status<- ifelse(data2plot3$status == "Positive", "Statement 1", ifelse(data2plot3$status == "Negative", "Statement 2", "Neither"))
  
  data2plot3$country <- as.factor(data2plot3$country)
  data2plot3<- data2plot3%>%
    mutate(country = fct_relevel(country, 
                            "Overall", "Kuwaiti", "Foreign"))
  
  # Applying plotting function
  chart <- divbars(data           = data2plot3,
                   target_var     = "perc",
                   grouping_var   = "country",
                   diverging_var  = "status",
                   negative_value = "It is more important \nto have a government \nthat can get things done, \neven if we have no influence \nover what it does.",
                   colors         = colors4plot,
                   labels     = "label",
                   lab_pos =  "lab_pos",
                   title = "Authoritarianism: Government Preference",
                   subtitle = "Percentage of people who think it is more important \nto have a government that can get things done compared \nto the percentage of people who think it is more important \nfor citizens to be able to hold the government accountable",
                   legend = "Statement",
                   categories_grouping_var = c("It is more important \nto have a government \nthat can get things done, \neven if we have no influence \nover what it does.","It is more important \nfor citizens to be able \nto hold government accountable, \neven if that means it makes \ndecisions more slowly.", "Agree with neither \nstatement.")
  )
  
  #ggsave("../Outcomes/Figure1/Figure1_1.svg", width = 100.8689, height = 15.464229, units  = "mm")
}


figure01_02.fn <- function(nchart = 3, PAR = F) {
  
  # Defining variables to use

    vars4plot <- c("q50")
  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    select(nation, all_of(vars4plot)) %>%
    mutate(
      across(!nation,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(nation) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(nation, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(nation, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(nation, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(country, group, status, perc, label) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("Positive", "Negative", "Neutral")
  
  
        
  data2plot$nation<- ifelse(data2plot$nation == 1, "Kuwaiti", "Foreign")
         
         # Applying plotting function
         chart <- divbars(data           = data2plot,
                              target_var     = "perc",
                              grouping_var   = "nation",
                              diverging_var  = "status",
                              negative_value = "Negative",
                              colors         = colors4plot,
                              labels     = "label",
                              lab_pos =  "lab_pos"
                              )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 100.8689,
                   h      = 15.464229)
         
         ggsave("../Outcomes/Figure1/Figure1_2.svg", width = 100.8689, height = 15.464229, units  = "mm")
}


figure01_02.fn <- function(nchart = 3, PAR = F) {
  
  # Defining variables to use
  if (PAR == F) {
    vars4plot <- c("q50", "q51", "q52", "CAR_q73", "CAR_q74")
  } else {
    vars4plot <- c("q50")
  }
  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    select(nation, all_of(vars4plot)) %>%
    mutate(
      across(!nation,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(nation) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(nation, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(nation, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(nation, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(country, group, status, perc, label) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette, "#A6A8AA")
  names(colors4plot) <- c("Positive", "Negative", "Neutral")
  
  
        
  data2plot$nation<- ifelse(data2plot$nation == 1, "Kuwaiti", "Foreign")
         
         # Applying plotting function
         chart <- divbars(data           = data2plot,
                              target_var     = "perc",
                              grouping_var   = "nation",
                              diverging_var  = "status",
                              negative_value = "Negative",
                              colors         = colors4plot,
                              labels     = "label",
                              lab_pos =  "lab_pos"
                              )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 100.8689,
                   h      = 15.464229)
         
         ggsave("../Outcomes/Figure1/Figure1_2.svg", width = 100.8689, height = 15.464229, units  = "mm")
}


figure02_01.fn <- function(nchart = 1) {
  
    vars4plot <- c("q7a", "q7b", "q7c")

  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    select(country, all_of(vars4plot)) %>%
    mutate(
      across(!country,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(country) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(country, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(country, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(country, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  
  # Customizing colorPalette for plot
  colors4plot <- c("#003b8a")
  names(colors4plot) <- c("Kuwait")
  
  
  
  data2plot$figures<- data2plot$country
  data2plot<- data2plot%>%
    filter(!is.na(country))
  data2plot$group<- ifelse(data2plot$group == "q7a", "Detailed budget figures of government agencies", 
                           ifelse(data2plot$group == "q7b", "Copies of government contracts", "Disclosure records of senior government officials"))
  data2plot<- data2plot%>%
    filter(status == "Negative")
  data2plot$perc<- (-1*data2plot$perc) 
  
  # Applying plotting function
  chart <- compared_bars(data.df      = data2plot,
                        value2plot     = "perc",
                   grouping_var   = "figures",
                   categories_grouping_var  = c("Overall"),
                   label_figures = "label",
                   colors4plot    = colors4plot,
                   labels_var     = "group",
                   order = F,
                   title = "Open Government: Information Requests",
                   subtitle = "Percentage of people who believe it is likely or very likely \nto recieve government information upon request",
                   legend = "Requested government \ninformation in the \nlast 12 months",
                   nbars = 2
  )
  
  ggsave("../Outcomes/Figure2/Figure2_1.svg", width = 185.8689, height = 76.23357, units  = "mm")
}

figure02_02.fn <- function(nchart = 1) {
  
  vars4plot <- c("q7a", "q7b", "q7c")
  
  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    select(q5, all_of(vars4plot)) %>%
    mutate(
      q5 = ifelse(q5 == 0, "No", ifelse(q5== 1, "Yes", NA_character_)),
      across(!q5,
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(all_of(vars4plot),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos"),
      across(all_of(vars4plot),
             ~if_else(.x == 5, 1, 0),
             .names = "{.col}_neither")
    ) %>%
    group_by(q5) %>%
    summarise(
      across(c(ends_with("_pos"),
               ends_with("_neg"),
               ends_with("_neither")),
             sum,
             na.rm = T)
    ) %>%
    mutate(
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_pos"),
      across(ends_with("_neither"),
             ~.x/2,
             .names = "{.col}_neg")
    ) %>%
    select(-ends_with("_neither"))
  
  # We need to dynamically generate the totals for each variable
  data2plot <- map_dfr(vars4plot,
                       function(categories) {
                         
                         data2plot %>%
                           select(q5, starts_with(categories)) %>%
                           mutate(
                             "{categories}_total" := rowSums(across(starts_with(categories)))
                           ) %>%
                           rename(total = ends_with("_total")) %>%
                           pivot_longer(!c(q5, total),
                                        values_to = "abs_value",
                                        names_to  = "category") %>%
                           mutate(
                             perc    = round((abs_value/total)*100, 
                                             0),
                             status  = case_when(
                               str_detect(category, "_neither") ~ "Neutral",
                               str_detect(category, "_neg")     ~ "Negative",
                               str_detect(category, "_pos")     ~ "Positive"
                             ),
                             status     = factor(status, levels = c("Negative", "Positive", "Neutral")),
                             perc       = if_else(str_detect(category, "_neg"), 
                                                  perc*-1, 
                                                  perc),
                             label      = paste0(format(abs(perc),
                                                        nsmall = 0),
                                                 "%"),
                             label      = if_else(status == "Neutral", NA_character_, label), 
                             group      = str_replace_all(category, "_pos|_neg|_neither", ""),
                             lab_status = case_when(
                               str_detect(category, "_pos") ~ "POS",
                               str_detect(category, "_neg") ~ "NEG"
                             )
                           )
                       }) %>%
    group_by(q5, group, lab_status) %>%
    mutate(lab_pos = sum(perc))
  
  
  # Customizing colorPalette for plot
  colors4plot <- c(binPalette)
  names(colors4plot) <- c("Yes", "No")
  
  
  
  data2plot$figures<- data2plot$q5
  data2plot<- data2plot%>%
    filter(!is.na(q5))
  data2plot$group<- ifelse(data2plot$group == "q7a", "Detailed budget figures of government agencies", 
                           ifelse(data2plot$group == "q7b", "Copies of government contracts", "Disclosure records of senior government officials"))
  data2plot<- data2plot%>%
    filter(status == "Negative")
  data2plot$perc<- (-1*data2plot$perc) 
  
  # Applying plotting function
  chart <- compared_bars(data.df      = data2plot,
                         value2plot     = "perc",
                         grouping_var   = "figures",
                         categories_grouping_var  = c("Yes", "No"),
                         label_figures = "label",
                         colors4plot    = colors4plot,
                         labels_var     = "group",
                         order = F,
                         title = "Open Government: Information Requests",
                         subtitle = "Percentage of people who believe it is likely or very likely to recieve \ngovernment information upon request based on whether or not \nthey have requested government information in the last 12 months",
                         legend = "Requested government \ninformation in the \nlast 12 months",
                         nbars = 2
  )
  
  ggsave("../Outcomes/Figure2/Figure2_2.svg", width = 225.8689, height = 76.23357, units  = "mm")
}

figure03_01.fn <- function(nchart = 1, PAR = F) {
  
  # Defining variables to include in plot
  vars4plot <- c("q8d", )
  
  # Country names or country codes?
  if (length(countrySet) > 4) {
    data_subset.df <- data_subset.df %>%
      mutate(country_name = country,
             country      = country_code)
  } else {
    data_subset.df <- data_subset.df %>%
      mutate(country_name = country)
  }
  
  # Defining data frame for plot
  data2plot <- data_subset.df %>%
    filter(year == latestYear) %>%
    select(country, 
           country_name, 
           all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(across(!c(country, country_name),
                  ~if_else(.x == 1 | .x == 2, 1, 
                           if_else(!is.na(.x) & .x != 99, 0, 
                                   NA_real_)))) %>%
    group_by(country, country_name) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!c(country, country_name),
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(value2plot  = value2plot*100,
           highlighted = if_else(country_name == mainCountry, "Highlighted", "Regular"),
           labels      = to_percentage.fn(value2plot))
  
  # Saving data points
  write.xlsx(as.data.frame(data2plot %>% select(!highlighted) %>% ungroup()), 
             file      = file.path("Outputs", 
                                   str_replace_all(mainCountry, " ", "_"),
                                   "dataPoints.xlsx",
                                   fsep = "/"), 
             sheetName = paste0("Chart_", nchart),
             append    = T,
             row.names = T)
  
  # Specifying a custom order for West Caribbean
  if (mainCountry %in% c(centralAmerica.ls, eastCaribbean.ls)) {
    c.order <- T
  } else {
    c.order <- F
  }
  if (mainCountry %in% eastCaribbean.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "BRB" ~ 1,
          country == "DMA" ~ 2,
          country == "GRD" ~ 3,
          country == "LCA" ~ 4,
          country == "VCT" ~ 5,
          country == "TTO" ~ 6
        )
      )
  }
  if (mainCountry %in% centralAmerica.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "BLZ" ~ 1,
          country == "CRI" ~ 2,
          country == "SLV" ~ 3,
          country == "GUA" ~ 4,
          country == "HND" ~ 5,
          country == "NIC" ~ 6,
          country == "PAN" ~ 7
        )
      )
  }
  if (mainCountry %in% g7.ls) {
    data2plot <- data2plot %>%
      mutate(
        order_var = case_when(
          country == "CAN" ~ 1,
          country == "FRA" ~ 2,
          country == "DEU" ~ 3,
          country == "ITA" ~ 4,
          country == "JPN" ~ 5,
          country == "GBR" ~ 6,
          country == "USA" ~ 7
        )
      )
  }
  
  if (! mainCountry %in% c(centralAmerica.ls, eastCaribbean.ls)) {
    data2plot <- data2plot %>%
      mutate(order_var = NULL)
  }
  
  # Defining colors
  colors4plot <- barsPalette
  names(colors4plot) <- c("Highlighted", "Regular")
  
  if (mainCountry %in% c(eastCaribbean.ls, 
                         westCaribbean_and_guianas.ls, 
                         southCone.ls, 
                         centralAmerica.ls,
                         g7.ls)){
    exp <- TRUE
  } else {
    exp <- FALSE
  }
  
  # Plotting each panel of Figure 5
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% tvar)
         
  
  # Applying plotting function
  chart <- barschart(data           = data2plot,
                   target_var     = "perc",
                   grouping_var   = "nation",
                   diverging_var  = "status",
                   negative_value = "Negative",
                   colors         = colors4plot,
                   labels     = "label",
                   lab_pos =  "lab_pos"
  )
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = panelName,
            w      = 100.8689,
            h      = 15.464229)
  
  ggsave("../Outcomes/Figure1/Figure1_2.svg", width = 100.8689, height = 15.464229, units  = "mm")
}
