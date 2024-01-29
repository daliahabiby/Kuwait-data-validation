## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Kuwait Presentation
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Monday 29th, 2024
##
## This version:      Monday 29th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
NM_dotsChart <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    sd_var,           # Variable that will supply the sd to the plot
    n_obs,            # Variable that will supply the observation number to the plot
    alpha = 0.05,            # Variable that will provide the level of confidence to the plot
    grouping_var,     # Variable containing the grouping values. Plot will show a different color per group.
    labels_var,       # Variable containing the Y-Axis labels to show in the plot
    colors,           # Named vector with the colors to apply to lines
    order_var,
    diffOpac = F,     # Should the dots have different opacity levels?
    opacities,        # Named vector with opacity levels
    diffShp = F,      # Should point be displayed using different shapes?
    shapes  = NA,      # Named vector with shapes to be displayed
    draw_ci = T,
    y_upper = 100,
    dsize = 4,
    fsize = 10,
    fsize2 = 10
){
  
  if (y_upper == 100) {
    upbound  <- 100
    ylimits  <- c(0,100)
    ybreaks  <- seq(0, 100, 20)
  }
  if (y_upper == 1) {
    upbound  <- 1
    ylimits  <- c(0,1)
    ybreaks  <- seq(0, 1, 0.2)
  }
  
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           sd_var        = all_of(sd_var),
           n_obs         = all_of(n_obs),
           grouping_var  = all_of(grouping_var),
           labels_var    = all_of(labels_var),
           order_var     = all_of(order_var))
  
  # Creating a strip pattern
  strips <- data %>%
    group_by(labels_var) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = upbound,
           xposition = rev(1:nrow(.)),
           xmin = xposition - 0.5,
           xmax = xposition + 0.5,
           fill = rep(c("grey", "white"), 
                      length.out = nrow(.))) %>%
    pivot_longer(c(xmin, xmax),
                 names_to  = "cat",
                 values_to = "x") %>%
    select(-cat) %>%
    filter(fill != "white")
  
  
  # Creating ggplot
  plt <- ggplot() +
    geom_blank(data       = data,
               aes(x      = reorder(labels_var, -order_var),
                   y      = target_var,
                   label  = labels_var,
                   color  = grouping_var))
  
  if (draw_ci == T){
    plt <- plt +
      geom_ribbon(data      = strips,
                  aes(x     = x,
                      ymin  = ymin,
                      ymax  = ymax,
                      group = xposition,
                      fill  = fill),
                  show.legend = F) +
      geom_errorbar(data    = data, 
                    aes(x   = reorder(labels_var, -order_var),
                        y   = target_var,
                        ymin  = target_var - qt(1- alpha/2, (n_obs - 1))*sd_var/sqrt(n_obs),
                        ymax  = target_var + qt(1- alpha/2, (n_obs - 1))*sd_var/sqrt(n_obs),
                        color = grouping_var, 
                        fill = NULL),
                    width = 0.2,  # Set the width of the error bars
                    show.legend = F)
  }
  
  plt <- plt +
    scale_fill_manual(values  = c("grey"  = "#EBEBEB",
                                  "white" = "#FFFFFF"),
                      na.value = NULL)
  
  if (diffShp == F) {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var, 
                       fill = NA),
                   fill   = NA,
                   size = dsize,
                   show.legend = T)
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       alpha = grouping_var),
                   size      = dsize,
                   show.legend    = F) +
        scale_alpha_manual(values = opacities)
    }
    
  } else {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var),
                   fill   = NA,
                   size   = dsize,
                   stroke = 2,
                   show.legend = F) +
        scale_shape_manual(values = shapes)
      
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var,
                       alpha = grouping_var),
                   fill   = NA,
                   size   = dsize,
                   stroke = 2,
                   show.legend    = F) +
        scale_shape_manual(values = shapes) +
        scale_alpha_manual(values = opacities)
    }
    
  }
  
  plt <- plt +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = ylimits,
                       breaks = ybreaks,
                       labels = paste0(ybreaks,
                                       "%"),
                       position = "right") +
    coord_flip() +
    WJP_theme() +
    theme(axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background   = element_blank(), 
          panel.ontop = T,
          axis.text.x = element_text(color = "#222221",
                                     hjust = 0,
                                     size  = fsize2),
          axis.text.y = element_text(color = "#222221",
                                     hjust = 0,
                                     size  = fsize))
  
  return(plt)
  
}

barsSecChart <- function(
    data  = data2plot,
    value = value2plot,
    label = label,
    order_value = order_value,
    fill_group = batch,
    figure = figure,
    title,
    subtitle,
    legend,
    colors4plot){
  
  ggplot(data,
         aes(
           x     = reorder(label, -order_value),
           y     = value,
           fill  = batch,
           label = figure, group = batch
         )) +
    geom_bar(stat = "identity",
             show.legend = F,
             position = position_dodge(widt = 0.9)) +
    geom_text(aes(y    = value + 5), 
              position = position_dodge(widt = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598)  +  
    labs(title    = title,
         subtitle = subtitle, 
         fill = legend) +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%"),
                       position = "right") +
    coord_flip() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size     = 0.25,
                                        colour   = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),
      strip.text.x = element_blank(),  # Adjusts facet labels to the left
      strip.background = element_blank(),  # Removes the grey box around facet labels
      strip.placement = "outside"
    ) + 
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 0),
                                            hjust = 0), 
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
    )
  
}

figure3 <- function(data){
  vars4plot = list("Assembly"                 = "q2a", 
                   "Police"                   = "q2d", 
                   "National Government"      = "q2b",
                   "Local Government"         = "q2c", 
                   "Judges & Magistrates"     = "q2e",
                   "Prosecutors"              = "q2f", 
                   "Public Defense Attorneys" = "q2g")
  
  # Defining data frame for plot
  data2plot <- data %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = TRUE))) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value2plot = round(value,2),
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%"),
           order_value =
             case_when(
               category == "Assembly" ~ 3,
               category == "Police" ~ 5,
               category == "National Government" ~ 4,
               category == "Local Government" ~ 1,
               category == "Judges & Magistrates" ~ 6,
               category == "Prosecutors" ~ 2,
               category == "Public Defense Attorneys" ~ 7
             ))
  
  corruption <- lollipop_chart(data2plot = data2plot, 
                               categories = category,
                               order_value = order_value) +
    guides(fill = FALSE) +
    labs(title = "Perceptions of Corruption by Institution",
         subtitle = "Percentage of respondents who believe that most or all people working in the following institutions are corrupt") +
    
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 10),
                                            hjust = 0), 
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
                                        hjust    = 0),
      legend.background = element_blank(),
      legend.box =  element_blank(), 
      legend.key = element_blank()
    )
    
  ggsave(plot     = corruption, 
         filename = paste0(path2SP, "/Presentations//Kuwait/Kuwait-data-validation/Outcomes/Figure3/corruptionActors.svg"),
         height   = 7.5, 
         width    = 12
         )
}

#figure4

figure4 <- function(data) {
  
  data2plot <- data %>%
    select(q46g_G2, q46c_G1, q46e_G2,
           q46d_G2, q46f_G1, q46a_G2,
           q46d_G1, q46e_G1, q46h_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(
      value  = value*100,
      figure = paste0(format(round(value, 0),
                             nsmall = 0),
                      "%"),
      label = 
        case_when(
          category == 'q46g_G2'       ~ paste("**Political parties**<br>can express opinions<br>",
                                              "against the<br>government"),
          category == 'q46c_G1'       ~ paste("**The media**<br>can express opinions<br>",
                                              "against the<br>government"),
          category == 'q46e_G2'       ~ paste("The media<br>can **expose cases<br>of corruption**"),
          category == 'q46d_G2'       ~ paste("People can<br>**attend community<br>meetings**"),
          category == 'q46f_G1'       ~ paste("People can<br>**join any political<br>organization**"),
          category == 'q46a_G2'       ~ paste("People can<br>**organize around an<br>issue or petition**"),
          category == 'q46d_G1'       ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**"),
          category == 'q46e_G1'       ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured"),
          category == 'q46h_G2'       ~ paste("Religious minorities<br>can **observe their<br>holy days**"),
        ),
      order_value = 
        case_when(
          category == 'q46g_G2'       ~ 9,
          category == 'q46c_G1'       ~ 10,
          category == 'q46e_G2'       ~ 11,
          category == 'q46e_G1'       ~ 2,
          category == 'q46d_G1'       ~ 3,
          category == 'q46f_G1'       ~ 6,
          category == 'q46d_G2'       ~ 7,
          category == 'q46a_G2'       ~ 8,
          category == 'q46h_G2'       ~ 13,
        ),
      batch =
        case_when(
          category %in% c("q46g_G2", "q46c_G1", "q46e_G2") ~ "Expression",
          category %in% c("q46d_G2", "q46f_G1", "q46a_G2") ~ "Participation",
          category %in% c("q46d_G1", "q46e_G1") ~ "Election",
          category %in% c("q46h_G2") ~ "Religion"
        )
    )
  
  a <- tibble(
    category = c(" ", " ", " ", " "),
    value = c(0, 0, 0, 0),
    figure = c(" ", " ", " ", " "),
    label = c("**| EXPRESSION**", "**| PARTICIPATION**", "**| ELECTION**", "**| RELIGION**"),
    order_value = c(8, 1, 5, 12),
    batch = c("Expression", "Participation", "Election", "Religion")
  )
  
  
  b <- bind_rows(data2plot, a)
  
  colors4plot <- c("#1a2589", 
                   "#a90099", 
                   "#3273ff",
                   "#ef4b4b")
  
  plot <- barsSecChart(data = b, 
                       value = value, 
                       title = "Perceptions of Fundamental Freedoms", 
                       subtitle = "Percentage of respondents who believe the following statements", 
                       legend = " ", 
                       colors4plot = colors4plot)
  
  ggsave(plot     = plot, 
         filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure4/FundamentalFreedoms.svg"),
         height   = 7.5, 
         width    = 12
  )
  
}

#figure5
figure5 <- function(data, nation = T){
  
  data2plot <- data %>%
    select(q46g_G2, q46c_G1, q46e_G2,
           q46d_G2, q46f_G1, q46a_G2,
           q46d_G1, q46e_G1, q46h_G2,
           nation, fin) %>%
    mutate(
      nation     =
        case_when(
          nation == 1 ~ "Native",
          nation == 2 ~ "Foreign"
        ),
      fin       =
        case_when(
          fin == 1 | fin == 2 | fin == 3 ~ "Insecure",
          fin == 4 | fin == 5 ~ "Secure"
        ),
      across(!c(nation,fin),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             ))
    ) %>%
    pivot_longer(!c(nation,fin),
                 names_to  = "category",
                 values_to = "value")
  if(nation == T) {
    
    data2plot <- data2plot %>%
      group_by(nation, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      )
    
  } else {
    
    data2plot <- data2plot %>%
      group_by(fin, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      drop_na()
    
  }
   data2plot <- data2plot %>%
    mutate(
      value  = mean_value*100,
      sd_value = sd_value*100,
      figure = paste0(format(round(value, 0),
                             nsmall = 0),
                      "%"),
      label = 
        case_when(
          category == 'q46g_G2'       ~ paste("**Political parties**<br>can express opinions<br>",
                                              "against the<br>government"),
          category == 'q46c_G1'       ~ paste("**The media**<br>can express opinions<br>",
                                              "against the<br>government"),
          category == 'q46e_G2'       ~ paste("The media<br>can **expose cases<br>of corruption**"),
          category == 'q46d_G2'       ~ paste("People can<br>**attend community<br>meetings**"),
          category == 'q46f_G1'       ~ paste("People can<br>**join any political<br>organization**"),
          category == 'q46a_G2'       ~ paste("People can<br>**organize around an<br>issue or petition**"),
          category == 'q46d_G1'       ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**"),
          category == 'q46e_G1'       ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured"),
          category == 'q46h_G2'       ~ paste("Religious minorities<br>can **observe their<br>holy days**"),
        ),
      order_value = 
        case_when(
          category == 'q46g_G2'       ~ 2,
          category == 'q46c_G1'       ~ 3,
          category == 'q46e_G2'       ~ 4,
          category == 'q46e_G1'       ~ 6,
          category == 'q46d_G1'       ~ 7,
          category == 'q46f_G1'       ~ 9,
          category == 'q46d_G2'       ~ 10,
          category == 'q46a_G2'       ~ 11,
          category == 'q46h_G2'       ~ 13,
        ),
      batch =
        case_when(
          category %in% c("q46g_G2", "q46c_G1", "q46e_G2") ~ "Expression",
          category %in% c("q46d_G2", "q46f_G1", "q46a_G2") ~ "Participation",
          category %in% c("q46d_G1", "q46e_G1") ~ "Election",
          category %in% c("q46h_G2") ~ "Religion"
        )
    )
  
   if(nation == T){
     
     a <- tibble(
       nation = c(" ", " ", " ", " "),
       category = c(" ", " ", " ", " "),
       value = c(NA_real_, NA_real_, NA_real_, NA_real_),
       figure = c(" ", " ", " ", " "),
       label = c("**| Expression**", "**| Participation**", "**| Election**", "**| Religion**"),
       order_value = c(1, 5, 8, 12),
       batch = c("Expression", "Participation", "Election", "Religion")
     )
     
     
   } else {
     
     a <- tibble(
       nation = c(" ", " ", " ", " "),
       category = c(" ", " ", " ", " "),
       value = c(NA_real_, NA_real_, NA_real_, NA_real_),
       figure = c(" ", " ", " ", " "),
       label = c("**| Expression**", "**| Participation**", "**| Election**", "**| Religion**"),
       order_value = c(1, 5, 8, 12),
       batch = c("Expression", "Participation", "Election", "Religion")
     )
     
   }
   
  b <- bind_rows(data2plot, a)
  colors4plot <- c("white","#ef4b4b","#1a2589")
  
  if(nation == T){
    
    plot <- NM_dotsChart(data         = b,
                         target_var   = "value",
                         sd_var       = "sd_value",
                         n_obs        = "n_obs", 
                         alpha        = 0.1,
                         grouping_var = "nation",
                         labels_var   = "label",
                         colors       = colors4plot,
                         order_var    = "order_value") +
      guides(fill = FALSE) +
      labs(title = "Perceptions of Fundamental Freedoms by Provenance",
           subtitle = "Percentage of respondents who believe the following statements",
           colour = "Provenance")
    
  } else {
    
    plot <- NM_dotsChart(data         = b,
                         target_var   = "value",
                         sd_var       = "sd_value",
                         n_obs        = "n_obs", 
                         alpha        = 0.1,
                         grouping_var = "fin",
                         labels_var   = "label",
                         colors       = colors4plot,
                         order_var    = "order_value") +
      guides(fill = FALSE) +
      labs(title = "Perceptions of Fundamental Freedoms by Financial Situation",
           subtitle = "Percentage of respondents who believe the following statements",
           colour = "Financial situation")
  } 
  
  plot <- plot +
    
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 10),
                                            hjust = 0), 
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
                                        hjust    = 0),
      legend.background = element_blank(),
      legend.box =  element_blank(), 
      legend.key = element_blank()
    )
  
  if(nation == T){
    
    ggsave(plot     = plot, 
           filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure4/FundamentalFreedomsNation.svg"),
           height   = 7.5, 
           width    = 12)
           
  } else {
    ggsave(plot     = plot, 
           filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure4/FundamentalFreedomsFIN.svg"),
           height   = 7.5, 
           width    = 12)
  }
  
}


#figure6
figure6 <- function(data){
  
  # Defining data frame for plot
  data2plot <- data %>%
    select(q49a, q49e_G2, q49e_G1, q49d_G1, q49c_G1) %>%
    mutate(
      
      # Transforming everything into binary variables
      across(everything(),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
    ) %>%
    select(q49a, q49e_G2, q49e_G1, q49d_G1, q49c_G1) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(
      order_value = case_when(
        category     == 'q49a'          ~ 2,
        category     == 'q49e_G2'       ~ 6,
        category     == 'q49e_G1'       ~ 3,
        category     == 'q49d_G1'       ~ 7,
        category     == 'q49c_G1'       ~ 4
      ),
      value = value*100,
      figure = paste0(round(value,0),"%"),
      label = case_when(
        category == 'q49a'          ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice"),
        category == 'q49e_G2'       ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                            "accused of<br>crimes as innocent<br>until proven guilty"),
        category == 'q49e_G1'       ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime"),
        category == 'q49d_G1'       ~ paste("Ensures **uniform quality** by<br>providing equal service<br>",
                                            "regardless of where<br>they live",
                                            "</span>"),
        category == 'q49c_G1'       ~ paste("Ensures everyone<br>has **access** to the<br>justice system"),
      ),
      across(label,
             ~paste0(
               "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
               label,
               "</span>")),
      batch =
        case_when(
          category %in% c("q49a", "q49e_G1", "q49c_G1") ~ "Achievements",
          category %in% c("q49e_G2", "q49d_G1")         ~ "Challenges"
        )
    )
  
  a <- tibble(
    category = c(" ", " "),
    value2plot = c(0, 0),
    figure = c(" ", " "),
    label = c("**| ACHIEVEMENTS**", "**| CHALLENGES**"),
    order_value = c(1, 5),
    batch = c("Achievements", "Challenges")
  )
  
  b <- bind_rows(data2plot, a)
  
  colors4plot <- c("#1a2589", 
                   "#ef4b4b")
  plot <- barsSecChart(data = b, 
                       value = value,
                       title = "Perceptions of the Criminal Justice System", 
                       subtitle = "Percentage of respondents who are confident that the criminal justice system...", 
                       legend = " ", 
                       colors4plot = colors4plot)
  ggsave(plot     = plot, 
         filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure5/CriminalJustice.svg"),
         height   = 7.5, 
         width    = 12)
  
  }


#Figure 7

figure7 <- function(data, nation = T){
  
  data2plot <- data %>%
    select(q49a, q49e_G2, q49e_G1, q49d_G1, q49c_G1,
           nation, fin) %>%
    mutate(
      nation     =
        case_when(
          nation == 1 ~ "Native",
          nation == 2 ~ "Foreign"
        ),
      fin       =
        case_when(
          fin == 1 | fin == 2 | fin == 3 ~ "Insecure",
          fin == 4 | fin == 5 ~ "Secure"
        ),
      # Transforming everything into binary variables
      across(!c(nation,fin),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
    ) %>%
    pivot_longer(!c(nation,fin),
                 names_to  = "category",
                 values_to = "value") 
    
    if(nation == T) {
      
      data2plot <- data2plot %>%
        group_by(nation, category) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n_obs = n()
        )
      
    } else {
      
      data2plot <- data2plot %>%
        group_by(fin, category) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n_obs = n()
        ) %>%
        drop_na()
      
    }
  
  data2plot <- data2plot %>%
    mutate(
      value  = mean_value*100,
      sd_value = sd_value*100,
      figure = paste0(format(round(value, 0),
                             nsmall = 0),
                      "%"),
      label = 
        case_when(
          category == 'q49a'          ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice"),
          category == 'q49e_G2'       ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                              "accused of<br>crimes as innocent<br>until proven guilty"),
          category == 'q49e_G1'       ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime"),
          category == 'q49d_G1'       ~ paste("Ensures **uniform quality** by<br>providing equal service<br>",
                                              "regardless of where<br>they live",
                                              "</span>"),
          category == 'q49c_G1'       ~ paste("Ensures everyone<br>has **access** to the<br>justice system"),
        ),
      order_value = 
        case_when(
          category     == 'q49a'          ~ 2,
          category     == 'q49e_G2'       ~ 6,
          category     == 'q49e_G1'       ~ 3,
          category     == 'q49d_G1'       ~ 7,
          category     == 'q49c_G1'       ~ 4
        ),
      batch =
        case_when(
          category %in% c("q49a", "q49e_G1", "q49c_G1") ~ "Achievements",
          category %in% c("q49e_G2", "q49d_G1")         ~ "Challenges"
        )
    )
  
  if(nation == T){
    
    a <- tibble(
      nation = c(" ", " "),
      category = c(" ", " "),
      mean_value = c(NA_real_, NA_real_),
      sd_value = c(0,0),
      n_obs = c(0,0),
      value = c(NA_real_, NA_real_),
      figure = c(" ", " "),
      label = c("**| ACHIEVEMENTS**", "**| CHALLENGES**"),
      order_value = c(1, 5),
      batch = c("Achievements", "Challenges")
    )
    
  } else {
    
    a <- tibble(
      fin = c(" ", " "),
      category = c(" ", " "),
      mean_value = c(NA_real_, NA_real_),
      sd_value = c(0,0),
      n_obs = c(0,0),
      value = c(NA_real_, NA_real_),
      figure = c(" ", " "),
      label = c("**| ACHIEVEMENTS**", "**| CHALLENGES**"),
      order_value = c(1, 5),
      batch = c("Achievements", "Challenges")
    )
  }
  
  b <- bind_rows(data2plot, a)
  colors4plot <- c("white","#ef4b4b","#1a2589")
  
  if(nation == T){
    
    plot <- NM_dotsChart(data         = b,
                         target_var   = "value",
                         sd_var       = "sd_value",
                         n_obs        = "n_obs", 
                         alpha        = 0.1,
                         grouping_var = "nation",
                         labels_var   = "label",
                         colors       = colors4plot,
                         order_var    = "order_value") +
      labs(title = "Perceptions of the Criminal Justice System by Provenance",
           subtitle = "Percentage of respondents who are confident that the criminal justice system...",
           colour = "Provenance")
    
  } else {
    
    plot <- NM_dotsChart(data         = b,
                         target_var   = "value",
                         sd_var       = "sd_value",
                         n_obs        = "n_obs", 
                         alpha        = 0.1,
                         grouping_var = "fin",
                         labels_var   = "label",
                         colors       = colors4plot,
                         order_var    = "order_value") +
      labs(title = "Perceptions of the Criminal Justice System by Financial Situation",
           subtitle = "Percentage of respondents who are confident that the criminal justice system...",
           colour = "Financial situation")
  }
  
  plot <- plot +
    guides(fill = FALSE) +
    labs(title = " ",
         subtitle = " ",
         colour = " ") +
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 10),
                                            hjust = 0), 
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
                                        hjust    = 0), 
      legend.background = element_blank(),
      legend.box =  element_blank(), 
      legend.key = element_blank()
    )
  
  if(nation == T){
    
    ggsave(plot     = plot, 
           filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure5/CriminalJusticeNation.svg"),
           height   = 7.5, 
           width    = 12)
    
  } else {
    ggsave(plot     = plot, 
           filename = paste0(path2SP, "/Presentations/Kuwait/Kuwait-data-validation/Outcomes/Figure5/CriminalJusticeFIN.svg"),
           height   = 7.5, 
           width    = 12)
  }
}
