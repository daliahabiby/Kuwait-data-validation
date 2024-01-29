## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Divbars function
##
## Author(s):         Dalia Habiby                (dhabiby@worldjusticeproject.org)
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


divbars <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    grouping_var,     # Variable containing the grouping values (Y-Axis Labels)
    diverging_var,    # Variable that contains the values to diverge,
    negative_value,   # Negative value showed in the diverging_var
    colors,           # Colors to apply to line
    labels_var,       # Variable containing the labels to show in the plot
    lab_pos,          # Variable containing the overall positioning of the label
    custom_order = F, # Do we want a customize order in the graph labels?
    order_var = NULL, # Variable containing the custom order for the labels
    extreme = F,       # Do we have extreme values?
    title,
    subtitle,
    legend,
    categories_grouping_var
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           diverging_var = all_of(diverging_var),
           labels_var    = all_of(labels_var),
           lab_pos       = all_of(lab_pos),
           order_var     = any_of(order_var))
  
  # Defining extra space for labels
  if (extreme == F){
    data <- data %>%
      mutate(added_space   = if_else(diverging_var == negative_value, -15, 15))
  } else {
    data <- data %>%
      mutate(added_space   = if_else(diverging_var == negative_value, -20, 20))
  }
  
  # Creating ggplot
  if (custom_order == F) {
    chart <- ggplot(data, aes(x     = grouping_var,
                              y     = target_var,
                              fill  = diverging_var,
                              label = labels_var))
  } else {
    chart <- ggplot(data, aes(x     = reorder(grouping_var, order_var),
                              y     = target_var,
                              fill  = diverging_var,
                              label = labels_var))
  }
  
  chart <- chart +
    geom_bar(stat        = "identity",
             position    = "stack",
             show.legend = T,
             width       = 0.85) +
    geom_text(aes(y = lab_pos + added_space),
              size     = 3.514598,
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    geom_hline(yintercept = 0,
               linetype   = "solid",
               size       = 0.5,
               color      = "#262424")+
    labs(title    = title,
         subtitle = subtitle, 
         fill = legend) + 
    scale_fill_manual(values = colors4plot, breaks = categories_grouping_var) 
  
  if (extreme == F){
    chart <- chart +
      scale_y_continuous(limits = c(-105,115))
  } else {
    chart <- chart +
      scale_y_continuous(limits = c(-110,125))
  }
  
  chart <- chart +
    scale_x_discrete(limits   = rev) +
    coord_flip() +
    WJP_theme() +
    theme(panel.grid.major = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          plot.title          = element_text(family   = "Lato Full",
                                             face     = "bold",
                                             size     = 4.920437*.pt,
                                             color    = "black",
                                             margin   = margin(0, 0, 10, 0),
                                             hjust    = 0), 
          plot.subtitle      = element_text(family   = "Lato Full",
                                            face     = "italic",
                                            size     = 4.217518*.pt,
                                            color    = "black",
                                            margin   = margin(2.5, 0, 20, 0),
                                            hjust    = 0)
          )+
    scale_fill_manual(name = "Statement", labels = c("Prefer a government that \ncan get things done \nover the ability to influence it.\n", "Prefer a government that \nis held accountable \nover efficiency\n", "Agree with neither \nstatement"), values = colors4plot)
  
  return(chart)
  
}
