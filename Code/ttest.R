ttest.fn <- function(data_set.df = data_subset.df,
                     dependent_vars,
                     independent_var,
                     grupo1,
                     grupo2) {
  
  results_list <- list()  # To store results for each dependent variable
  
  for (dep_var in dependent_vars) {
    
    # Rename columns in the data_set.df
    data2table <- data_set.df %>%
      rename(
        dependent_var   = !!ensym(dep_var),
        independent_var = !!ensym(independent_var)
      )
    
    # Extract unique values of the independent variable for the specified groups
    independent_vars <- data2table %>%
      filter(independent_var %in% c(grupo1, grupo2)) %>%
      pull(independent_var) %>%
      unique() %>%
      sort()
    
    # Extract and print the data for group1
    group1 <- data2table %>%
      filter(independent_var == grupo1) %>%
      pull(dependent_var)
    n_grupo1 <- length(group1)
    mean_grupo1 <- mean(group1, na.rm = TRUE)
    
    # Extract and print the data for group2
    group2 <- data2table %>%
      filter(independent_var == grupo2) %>%
      pull(dependent_var)
    n_grupo2 <- length(group2)
    mean_grupo2 <- mean(group2, na.rm = TRUE)
    
    # Perform t-test between group1 and group2
    ttest <- t.test(group1, group2)
    
    # Store results in a tibble
    result_df <- tibble(
      "dependent_var"      = paste0(dep_var),
      "independent_var"    = paste0({{independent_var}}),
      "grupo1"             = paste0(grupo1),
      "grupo1_mean"        = mean_grupo1,
      "grupo1_nobs"        = n_grupo1,
      "grupo2"             = paste0(grupo2),
      "grupo2_mean"        = mean_grupo2,
      "grupo2_nobs"        = n_grupo2,
      "diff_mean"          = mean_grupo1 - mean_grupo2,
      "direccion"          = if_else(mean_grupo1 - mean_grupo2 > 0, "Positiva", "Negativa"),
      "pvalue"             = ttest$p.value,
      "significativo10"    = if_else(ttest$p.value < 0.1, "Si", "No"),
      "significativo5"     = if_else(ttest$p.value < 0.05, "Si", "No"),
      "significativo1"     = if_else(ttest$p.value < 0.01, "Si", "No")
    )
    
    # Add result_df to the results_list
    results_list[[dep_var]] <- result_df
  }
  
  # Combine all dataframes in the results_list into a single dataframe
  final_result_df <- bind_rows(results_list)
  
  return(final_result_df)
}
