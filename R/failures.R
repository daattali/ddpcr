## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Analysis step: Remove failed wells
#' 
#' Check if any wells have failed the ddPCR experiment by checking a series
#' of quality control metrics.  If any well is deemed as a failure, all the droplets
#' in that well will be assigned to the \emph{FAILED} cluster.\cr\cr
#' \href{https://github.com/daattali/ddpcr#advanced-topic-2-algorithms-used-in-each-step}{See the README} for
#' more information about the algorithm used to find failed wells.
#' 
#' This function is recommended to be run as part of an analysis pipeline (ie.
#' within the \code{\link[ddpcr]{analyze}} function) rather than being called
#' directly.
#' 
#' @param plate A ddPCR plate.
#' @return A ddPCR plate with the droplets in failed wells marked as failed. The plate's
#' metadata will have a new variable \code{success} which will be \code{FALSE}
#' for any failed well and \code{TRUE} for all others.
#' @seealso \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{is_well_success}}
#' @note This is an S3 generic, which means that different ddPCR plate types can
#' implement this function differently. 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on how to implement custom ddPCR plate types.
#' @export
#' @keywords internal
remove_failures <- function(plate) {
  UseMethod("remove_failures")
}

#' Analysis step: Remove failed wells
#' @inheritParams remove_failures
#' @export
#' @keywords internal
remove_failures.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step('REMOVE_FAILURES')
  plate %>% check_step(CURRENT_STEP)
  step_begin("Identifying failed wells")
  
  data <- plate_data(plate)
  
  # ---
  
  # check every well to see if it failed
  well_success_map <-
    vapply(
      wells_used(plate),
      function(x) is_well_success(plate, x),
      logical(1)
    ) %>%
    named_vec_to_df("success")
  
  # add the success/failed status of each well to the plate's metadata
  meta <-
    merge_dfs_overwrite_col(plate_meta(plate),
                            well_success_map,
                            "success") %>%
    arrange_meta
  
  # set the cluster to failed for every droplet in a failed well
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

#' Determine if a well had a successful ddPCR run
#' 
#' This function runs the actual algorithm for determining which wells failed.
#' @return \code{FALSE} if there are obvious quality problems with the well that
#' suggest the ddPCR run failed; \code{TRUE} otherwise.
#' @export
#' @keywords internal
is_well_success <- function(plate, well_id) {
  UseMethod("is_well_success")
}

#' Determine if a well had a successful ddPCR run
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
  
  set.seed(params(plate, 'GENERAL', 'RANDOM_SEED'))
  
  # fit two clusters into the 2D data
  kmeans <- stats::kmeans(well_data, 2)
  
  # extract the centers of the two clusters as a 'point2d' object
  centers <- kmeans$centers %>% t %>% as.data.frame %>% lapply(point2d)
  # calculate the distance from each cluster center to the origin
  distances <- lapply(centers, diff) %>% unlist
  # determine which cluster is closer to the origin
  smaller_center_idx <- distances %>% which.min
  
  # Check if the two cluster centers are very close to each other
  # if they are too close, it indicates that the two clusters are really the same
  if (diff(centers[[1]], centers[[2]]) < diff(centers[[smaller_center_idx]])) {
    return(FALSE)
  }
  
  # lambda = fraction of drops that are in the cluster
  smaller_lambda <- kmeans$size[[smaller_center_idx]] / sum(kmeans$size)
  
  # Make sure we found a significant empty cluster
  if (smaller_lambda < params(plate, 'REMOVE_FAILURES', 'EMPTY_LAMBDA_LOW_T')) {
    return(FALSE)
  }  
  
  # Make sure not too many drops are empty
  if (smaller_lambda > params(plate, 'REMOVE_FAILURES', 'EMPTY_LAMBDA_HIGH_T')) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Get successful/failed wells
#' 
#' Get a list of wells that had successful or failed ddPCR runs. One of the analysis steps
#' for ddPCR plates includes identifying failed wells, which are wells where
#' the ddPCR run was not successful and did not produce useful droplet data.
#' @param plate A ddPCR plate
#' @return List of wells that had a successful/failed ddPCR run.
#' @seealso \code{\link[ddpcr]{remove_failures}}
#' @examples 
#' \dontrun{
#' dir <- sample_data_dir()
#' plate <- new_plate(dir) %>% analyze
#' plate %>% wells_success
#' plate %>% wells_failed
#' } 
#' @name wells_success
NULL

#' @rdname wells_success
#' @export
wells_success <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!(plate %>% has_step('REMOVE_FAILURES')) ||
      plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return(plate %>% plate_meta %>% .[['well']])
  }
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ success) %>%
    .[['well']]
}
#' @rdname wells_success
#' @export
wells_failed <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!(plate %>% has_step('REMOVE_FAILURES')) ||
      plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return()
  }  
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ !success) %>%
    .[['well']]
}
