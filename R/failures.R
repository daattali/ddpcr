## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# Determine if a well had a successful ddPCR run
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .plot: If true, plot the result (used mainly for development/debugging)
#
# Returns:
#   list:
#     success: TRUE if the well had a successful run, FALSE otherwise
#
# Algorithm:
#   The goal here is to see if there is anything that is clearly wrong with the
#   data in the well.  First, I ensure enough drops were loaded (BioRad claims 
#   20k, we see 12k-17k usually) - less than 5000 (PARAMS$REMOVE_FAILURES$TOTAL_DROPS_T)
#   is considered a failed run. Next I make a few basic sanity checks about the 
#   general look of the data in clusters.
#   If we were to normalize all HEX and FAM values to be between 0-1, we'd expect to
#   always have a very large cluster of empty drops near (0,0), a cluster of
#   wild-type drops near (1,1), and possibly a cluster of mutant drops neat (0,1).
#   Using these assumptions on the data, we make the following three heuristic
#   checks after fitting a mixture of 2 normal distributions in the FAM values
#   of all the drops:
#     - The centers (mu's) of the two populations need to be far enough (this
#       if checked by seeing if the larger center is at least twice the value of the
#       lower center.  If it is not, that usually indicates that both normal populations
#       are really the same cluster)
#     - The lower population cannot have too few drops (lambda < 0.3)
#       (PARAMS$REMOVE_FAILURES$NORMAL_LAMBDA_LOW_T), as that means there is no
#       empty cluster.  Likewise, there shouldn't be too many drops (lambda > 0.99)
#       (PARAMS$REMOVE_FAILURES$NORMAL_LAMBDA_HIGH_T), as that means there are not
#       enough drops with data
#     - The standard deviation (sigma) of the lower population should be fairly
#       small, below 200 (PARAMS$REMOVE_FAILURES$NORMAL_SIGMA_T), to ensure that
#       the empty cluster is indeed very dense as it should be
#' Determine is a well was a success or failure
#' @export
#' @keywords internal
is_well_success <- function(plate, well_id) {
  UseMethod("is_well_success")
}

#' Algorithm for determining if a single well failed
#' 
#' @export
#' @keywords internal
is_well_success.ddpcr_plate <- function(plate, well_id) {
  
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
  
  # Use kmeans to fit two clusters into the 2D data
  kmeans <- kmeans(well_data, 2)
  centers <- kmeans$centers %>% t %>% as.data.frame %>% lapply(point2d)
  distances <- lapply(centers, diff) %>% unlist
  smaller_center_idx <- distances %>% which.min
  
  # Check if the two cluster centers are very close to each other
  if (diff(centers[[1]], centers[[2]]) < diff(centers[[smaller_center_idx]])) {
    return(FALSE)
  }
  
  smaller_lambda <- kmeans$size[[smaller_center_idx]] / sum(kmeans$size)
  
  # Make sure we found a significant empty cluster
  if (smaller_lambda < params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_LOW_T')) {
    return(FALSE)
  }  
  
  # Make sure not too many drops are empty
  if (smaller_lambda > params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_HIGH_T')) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Remove failed wells
#' @export
remove_failures <- function(plate) {
  UseMethod("remove_failures")
}

#' Removing failed wells
#' 
#' The algorithm for removing failed wells from a plate
#' 
#' @export
#' @keywords internal
remove_failures.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step('REMOVE_FAILURES')
  plate %>% check_step(CURRENT_STEP, TRUE)
  step_begin("Identifying failed wells")
  
  data <- plate_data(plate)
  
  # ---

  well_success_map <-
    vapply(
      wells_used(plate), # check every well if it failed
      function(x) is_well_success(plate, x),
      logical(1)
    ) %>%
    named_vec_to_df("success")

  meta <-
    merge_dfs_overwrite_col(plate_meta(plate),
                            well_success_map,
                            "success") %>%
    arrange_meta

  CLUSTER_FAILED <- plate %>% cluster('FAILED')
  failed_wells <-
    well_success_map %>%
    dplyr::filter_(~ !success) %>%
    .[['well']]
  failed_idx <- 
    (data[['well']] %in% failed_wells) & (data[['cluster']] <= CLUSTER_FAILED)
  data[failed_idx, 'cluster'] <- CLUSTER_FAILED  
    
  # ---
  
  plate_meta(plate) <- meta
  plate_data(plate) <- data
  status(plate) <- CURRENT_STEP
  step_end()

  plate
}
