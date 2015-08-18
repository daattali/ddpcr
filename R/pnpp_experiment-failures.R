is_well_success.pnpp_experiment <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(NA)
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params(plate, 'REMOVE_FAILURES', 'TOTAL_DROPS_T')) {
    return(FALSE)
  }
  
  set.seed(params(plate, 'GENERAL', 'RANDOM_SEED'))
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  kmeans_y <- kmeans(well_data[[y_var]], 2, nstart = 5)
  centers_y <- kmeans_y$centers %>% as.integer
  smaller_comp_y <- centers_y %>% which.min
  
  if ((centers_y %>% diff %>% abs) < min(centers_y)) {
    return(FALSE)
  }
  
  smaller_lambda <- kmeans_y$size[smaller_comp_y]/sum(kmeans_y$size)
  
  if (smaller_lambda < params(plate, 'REMOVE_FAILURES', 'EMPTY_LAMBDA_LOW_T')) {
    return(FALSE)
  }
  
  if (smaller_lambda > params(plate, 'REMOVE_FAILURES', 'EMPTY_LAMBDA_HIGH_T')) {
    return(FALSE)
  }
  
  return(TRUE) 
}