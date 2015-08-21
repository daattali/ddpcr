## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Analysis step: Reclassify droplets
#' 
#' After classifying droplets into clusters in each well individually, it may
#' be useful to attempt to reclassify droplets in wells where the gates are
#' not very clearly defined. This function uses information taken from wells
#' with a high negative frquency (where the gate is easily defined) to adjust
#' the gates in the other wells.\cr\cr
#' \href{https://github.com/daattali/ddpcr#algorithm}{See the README} for
#' more information about the algorithm used.
#' 
#' This function is recommended to be run as part of an analysis pipeline (ie.
#' within the \code{\link[ddpcr]{analyze}} function) rather than being called
#' directly.
#' @seealso \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{reclassify_droplets_single}}\cr
#' \code{\link[ddpcr]{mark_clusters}}
#' 
#' @note This is an S3 generic, which means that different ddPCR plate types can
#' implement this function differently. 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on how to implement custom ddPCR plate types.
#' @export
#' @keywords internal
reclassify_droplets <- function(plate) {
  UseMethod("reclassify_droplets")
}

#' Analysis step: Reclassify droplets
#' @inheritParams reclassify_droplets
#' @export
#' @keywords internal
reclassify_droplets.pnpp_experiment <- function(plate) {
  CURRENT_STEP <- plate %>% step('RECLASSIFY')
  plate %>% check_step(CURRENT_STEP)
  
  # if there are not enough wells with high MT freq to use as prior info or if
  # there are no wells with low MT freq to reclassify, do nothing
  min_wells <- params(plate, 'RECLASSIFY', 'MIN_WELLS_NEGATIVE_CLUSTER')
  if (plate %>% wells_negative %>% length < min_wells ||
      plate %>% wells_positive %>% length == 0) {
    message(paste0("Reclassifying droplets... aborted because there are not enough",
                   " wells with significant ",
                   params(plate, 'GENERAL', 'NEGATIVE_NAME'),
                   " clusters"))
    status(plate) <- CURRENT_STEP
    return(plate)
  }
  
  step_begin("Reclassify droplets based on info in all wells")
  
  data <- plate_data(plate)
  CLUSTER_NEGATIVE <- plate %>% cluster('NEGATIVE')
  CLUSTER_POSITIVE <- plate %>% cluster('POSITIVE')
  
  # calculate the ratio of the highest MT drop over the median WT drop
  variable_var <- variable_dim_var(plate)
  consensus_border_ratio <-
    vapply(plate %>% wells_negative,
           function(x) {
             negative_max <- 
               data %>%
               dplyr::filter_(~ well == x,
                              ~ cluster == CLUSTER_NEGATIVE) %>%
               .[[variable_var]] %>%
               max
             positive_median <- 
               data %>%
               dplyr::filter_(~ well == x,
                              ~ cluster == CLUSTER_POSITIVE) %>%
               .[[variable_var]] %>%
               median               
             ratio <- negative_max / positive_median
             ratio
           },
           numeric(1)
    ) %>%
    quantile(params(plate, 'RECLASSIFY', 'BORDER_RATIO_QUANTILE')) %>%
    as.numeric
  
  wells_to_reclassify <- plate %>% wells_positive
  
  well_clusters_info <-
    vapply(wells_to_reclassify,
           function(x) reclassify_droplets_single(plate, x, consensus_border_ratio = consensus_border_ratio),
           integer(1)) %>%
    named_vec_to_df(meta_var_name(plate, "negative_border"))
  
  # add metadata to each well
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(well_clusters_info)  
  
  plate %<>%
    mark_clusters(wells_to_reclassify) %>%
    calculate_negative_freqs
  
  # ---
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}

#' Reclassify droplets in a well 
#' @keywords internal
#' @export
reclassify_droplets_single <- function(plate, well_id, ...) {
  UseMethod("reclassify_droplets_single")
}

#' Reclassify droplets in a well 
#' 
#' Reclassify droplets in a well given the ratio of where to place the MT border
#' over the median WT drops
#' @keywords internal
#' @export
reclassify_droplets_single.pnpp_experiment <- function(plate, well_id, ..., consensus_border_ratio) {
  
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  variable_var <- variable_dim_var(plate)
  
  # find the median WT drops
  CLUSTER_POSITIVE <- plate %>% cluster('POSITIVE')
  positive_median <- 
    well_data %>%
    dplyr::filter_(~ cluster == CLUSTER_POSITIVE) %>%
    .[[variable_var]] %>%
    median    
  
  # determine the negative border based on the median WT drops and the given ratio
  negative_border <- (consensus_border_ratio * positive_median) %>% as.integer
  
  negative_border
}
