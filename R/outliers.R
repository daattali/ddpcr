## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Analysis Step: Remove outlier droplets
#' 
#' Identify droplets that have an abnormally high fluorescence intensity as
#' outliers. Any such droplets will be assigned to the \emph{OUTLIER} cluster.\cr\cr
#' \href{https://github.com/daattali/ddpcr#algorithm}{See the README} for
#' more information about the algorithm used to find outlier droplets.
#' 
#' This function is recommended to be run as part of an analysis pipeline (ie.
#' within the \code{\link[ddpcr]{analyze}} function) rather than being called
#' directly.
#' 
#' @param plate A ddPCR plate.
#' @return A ddPCR plate with outlier droplets marked as outliers. The plate's
#' metadata will have a new variable \code{drops_outlier} which will count the
#' number of outlier droplets in each well.
#' @seealso \code{\link[ddpcr]{analyze}}
#' \code{\link[ddpcr]{get_outlier_cutoff}}
#' @note This is an S3 generic, which means that different ddPCR plate types can
#' implement this function differently. 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on how to implement custom ddPCR plate types.
#' @export
#' @keywords internal
remove_outliers <- function(plate) {
  UseMethod("remove_outliers")
}

#' Analysis Step: Remove outlier droplets
#' @inheritParams remove_outliers
#' @export
#' @keywords internal
remove_outliers.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step('REMOVE_OUTLIERS')
  plate %>% check_step(CURRENT_STEP)
  step_begin("Identifying outlier droplets")
  
  data <- plate_data(plate)
  
  # ---
  
  # get the cutoff for outliers for the whole plate in each dimension
  outlier_cutoff <- plate %>% get_outlier_cutoff
  cutoff_x <- outlier_cutoff[[x_var(plate)]]
  cutoff_y <- outlier_cutoff[[y_var(plate)]]
  
  # assign the OUTLIER cluster to any drops that have a fluorescence value
  # above the cutoff
  CLUSTER_OUTLIER <- plate %>% cluster('OUTLIER')
  outlier_idx <-
    (data[[y_var(plate)]] > cutoff_y | data[[x_var(plate)]] > cutoff_x)
  data[outlier_idx, 'cluster'] <- CLUSTER_OUTLIER  
  
  # count how many outlier drops are in each well and add it to the metadata
  drops_outlies_df <- dplyr::data_frame(
    "well" = plate %>% wells_used,
    "drops_outlier" = 0L)  
  
  meta <-
    data %>%
    dplyr::filter_(~ cluster == CLUSTER_OUTLIER) %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops_outlier" = ~ n()) %>%
    merge_dfs_overwrite_col(drops_outlies_df, ., "drops_outlier") %>%
    merge_dfs_overwrite_col(plate_meta(plate), ., "drops_outlier")
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}

#' Get the cutoff for outliers
#' @return A named list with two elements, giving the cutoff for outliers in
#' each dimension.
#' @export
#' @keywords internal
get_outlier_cutoff <- function(plate) {
  UseMethod("get_outlier_cutoff")
}

#' Get the cutoff for outliers
#' @export
#' @keywords internal
get_outlier_cutoff.ddpcr_plate <- function(plate) {
  data <-
    plate_data(plate) %>%
    dplyr::filter_(~ well %in% wells_success(plate))
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)

  # get the top 1% of values in the Y dimension
  top_y <- 
    sort(data[[y_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'REMOVE_OUTLIERS', 'TOP_PERCENT'))
  # define the cutoff as the third quantile of the aforementioned top 1%
  # plus 5 IQR
  q_y <- quantile(top_y, c(.25, .75))
  cutoff_y <-
    (diff(q_y) * params(plate, 'REMOVE_OUTLIERS', 'CUTOFF_IQR') + q_y[2]) %>%
    as.numeric
  
  # repeat above with the X dimension
  top_x <- 
    sort(data[[x_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'REMOVE_OUTLIERS', 'TOP_PERCENT'))
  q_x <- quantile(top_x, c(.25, .75))
  cutoff_x <-
    (diff(q_x) * params(plate, 'REMOVE_OUTLIERS', 'CUTOFF_IQR') + q_x[2]) %>%
    as.numeric
  
  result <- list()
  result[[x_var]] <- cutoff_x
  result[[y_var]] <- cutoff_y
  
  result
}