parent_plate_type.wtnegbraf <- function(plate) {
  "ppnp_assay"
}

default_params.wtnegbraf <- function(plate) {
  params <- NextMethod("default_params")
  
  params[['GENERAL']][['X_VAR']] <- "HEXXX"
  params[['GENERAL']][['Y_VAR']] <- "FAMMM"
  params[['GENERAL']][['POSITIVE_NAME']] <- 'wildtype'
  params[['GENERAL']][['NEGATIVE_NAME']] <- 'mutant'
  params[['GENERAL']][['POSITIVE_DIMENSION']] <- 'Y'
    
  params
}

is_well_success.ppnp_assay <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(list(success = NA, comment = NA))
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params(plate, 'WELLSUCCESS', 'TOTAL_DROPS_T')) {
    success <- FALSE
    msg <- sprintf("Not enough drops generated (%s)", nrow(well_data))
    return(list(success = success, comment = msg))
  }
  
  set.seed(SEED)
  
  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR')
  
  if (params(plate, 'WELLSUCCESS', 'FAST')) {
    kmeans_y <- kmeans(well_data[[Y_var]], 2, nstart = 5)
    centers_y <- kmeans_y$centers %>% as.integer
    smaller_comp_y <- centers_y %>% which.min
    
    if ((centers_y %>% diff %>% abs) < min(centers_y)) {
      success <- FALSE
      msg <- sprintf("There seems to be mostly empty drops (centers of %s clusters: %s)",
                     Y_var, paste0(centers_y, collapse = ","))
      return(list(success = success, comment = msg))
    }
    
    smaller_lambda <- kmeans_y$size[smaller_comp_y]/sum(kmeans_y$size)
    
    if (smaller_lambda < params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_LOW_T')) {
      success <- FALSE
      msg <- paste0("Could not find significant empty cluster (lambda of ", Y_var, " normal: ",
                    signif(smaller_lambda, 4), ")")
      return(list(success = success, comment = msg))
    }
    
    if (smaller_lambda > params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_HIGH_T')) {
      success <- FALSE
      msg <- paste0("There are too many empty drops (lambda of ", Y_var, " normal: ",
                    signif(smaller_lambda, 4), ")")
      return(list(success = success, comment = msg))
    }
    
    return(list(success = TRUE, comment = NA)) 
  }
  
  # fit two normal distributions in the data along the Y dimension, check:
  # - the mu's of the two populations needs to be far enough
  # - bottom population needs to have lambda not too small and not too large
  # - sigma of bottom population should be fairly small  
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[Y_var]], k = 2))
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  larger_comp_y <- mixmdl_y$mu %>% which.max
  
  if ((mixmdl_y$mu %>% diff %>% abs) < min(mixmdl_y$mu)) {
    success <- FALSE
    msg <- paste0("There seems to be mostly empty drops (mu's of ", Y_var, " normals: ",
                  paste0(round(mixmdl_y$mu), collapse = " "), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] < params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_LOW_T')) {
    success <- FALSE
    msg <- paste0("Could not find significant empty cluster (lambda of ", Y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] > params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_HIGH_T')) {
    success <- FALSE
    msg <- paste0("There are too many empty drops (lambda of ", Y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$sigma[smaller_comp_y] > params(plate, 'WELLSUCCESS', 'NORMAL_SIGMA_T')) {
    success <- FALSE
    msg <- paste0("Could not find a dense empty cluster (sigma of ", Y_var, " normal: ",
                  round(mixmdl_y$sigma[smaller_comp_y]), ")")
    return(list(success = success, comment = msg))
  }    
  
  # if all the sanity checks passed, the run was successful
  return(list(success = TRUE, comment = NA))    
}

#' @export
wells_mutant <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ has_mt_cluster) %>%
    .[['well']]
}

#' @export
wells_wildtype <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ !has_mt_cluster) %>%
    .[['well']]
}