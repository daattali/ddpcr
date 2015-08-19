## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Remove empty droplets
#' @inheritParams remove_empty
#' @export
#' @keywords internal
remove_empty.pnpp_experiment <- function(plate) {
  # make sure the POSITIVE_DIM parameter is set (either the user manually set
  # it, or by using a subtype that has a default)
  if (is.na(positive_dim(plate))) {
    err_msg("You cannot analyze a `pnpp_experiment` without setting the `positive_dim` parameter.")
  }
  NextMethod("remove_empty")
}

#' Get the cutoff for empty droplets in a well
#' 
#' Very similar to the \code{get_empty_cutoff} method of the base plate type,
#' except for this plate type we know that we don't expect any droplets in one
#' of the corners, so we can save time by only having an empty cutoff in one
#' dimension.
#' @export
#' @keywords internal
get_empty_cutoff.pnpp_experiment <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  positive_var <- positive_dim_var(plate)
  
  set.seed(params(plate, 'GENERAL', 'RANDOM_SEED'))

  # fit two normal distributions in the data along the y dimension
  quiet(
    mixmdl_pos <- mixtools::normalmixEM(well_data[[positive_var]], k = 2))
  
  # set the Y cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_pos <- mixmdl_pos$mu %>% which.min
  cutoff_pos <-
    (mixmdl_pos$mu[smaller_comp_pos] +
       params(plate, 'REMOVE_EMPTY', 'CUTOFF_SD') * mixmdl_pos$sigma[smaller_comp_pos]) %>%
    ceiling %>%
    as.integer
  
  res <- list()
  res[[positive_dim(plate) %>% tolower]] <- cutoff_pos
  res[[variable_dim(plate) %>% tolower]] <- NA
  return(res) 
}
