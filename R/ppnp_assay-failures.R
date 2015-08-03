is_well_success.ppnp_assay <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(NA)
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params(plate, 'REMOVE_FAILURES', 'TOTAL_DROPS_T')) {
    return(FALSE)
  }
  
  set.seed(SEED)
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  if (params(plate, 'REMOVE_FAILURES', 'FAST')) {
    kmeans_y <- kmeans(well_data[[y_var]], 2, nstart = 5)
    centers_y <- kmeans_y$centers %>% as.integer
    smaller_comp_y <- centers_y %>% which.min
    
    if ((centers_y %>% diff %>% abs) < min(centers_y)) {
      return(FALSE)
    }
    
    smaller_lambda <- kmeans_y$size[smaller_comp_y]/sum(kmeans_y$size)
    
    if (smaller_lambda < params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_LOW_T')) {
      return(FALSE)
    }
    
    if (smaller_lambda > params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_HIGH_T')) {
      return(FALSE)
    }
    
    return(TRUE) 
  }
  
  # fit two normal distributions in the data along the Y dimension, check:
  # - the mu's of the two populations needs to be far enough
  # - bottom population needs to have lambda not too small and not too large
  # - sigma of bottom population should be fairly small  
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[y_var]], k = 2))
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  larger_comp_y <- mixmdl_y$mu %>% which.max
  
  if ((mixmdl_y$mu %>% diff %>% abs) < min(mixmdl_y$mu)) {
    return(FALSE)
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] < params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_LOW_T')) {
    return(FALSE)
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] > params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_HIGH_T')) {
    return(FALSE)
  }
  
  if (mixmdl_y$sigma[smaller_comp_y] > params(plate, 'REMOVE_FAILURES', 'NORMAL_SIGMA_T')) {
    return(FALSE)
  }    
  
  # if all the sanity checks passed, the run was successful
  return(TRUE) 
}