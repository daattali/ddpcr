# Determines the cutoffs for outliers on the plate.
# Note this function is currently only being used on the whole plate and
# thus determines outliers on a plate-level, but it can also be used to find
# outliers in each well by passing only data from a single well
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   list:
#     HEX: the HEX value of the outlier cutoff
#     FAM: the FAM value of the outlier cutoff
#
# Algorithm:
#   The idea borrows from outlier detection in normal populations (looking for
#   points that are further than k*IQR from the 1st/3rd quartiles), but since
#   our data is highly skewed and non-normal, I use a small tweak.
#   For each dimension ([FAM, HEX]): get the 1% (PARAMS$OUTLIERS$TOP_PERCENT) of
#   drops that have the highest value in that dimension.  Calculate the IQR of the
#   values only within these drops. Mark the outlier cutoff as the 3rd quantile
#   plus 5 (PARAMS$OUTLIERS$CUTOFF_IQR) IQR
#' Get the cutoff for outliers
#' 
#' @export
#' @keywords internal
get_outlier_cutoff <- function(plate) {
  UseMethod("get_outlier_cutoff")
}

#' Algorithm for determining the cutoff for outliers
#' 
#' @export
#' @keywords internal
get_outlier_cutoff.ddpcr_plate <- function(plate) {
  data <-
    plate_data(plate) %>%
    dplyr::filter_(~ well %in% wells_success(plate))
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  top_y <- 
    sort(data[[y_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_y <- quantile(top_y, c(.25, .75))
  cutoff_y <-
    (diff(q_y) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_y[2]) %>%
    as.numeric
  
  top_x <- 
    sort(data[[x_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_x <- quantile(top_x, c(.25, .75))
  cutoff_x <-
    (diff(q_x) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_x[2]) %>%
    as.numeric
  
  result <- list()
  result[[x_var]] <- cutoff_x
  result[[y_var]] <- cutoff_y
  
  result
}

# Removes outlier drops from a plate
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   Dataframe with outliers removed
#' Remove outlier droplets
#' @export
remove_outliers <- function(plate) {
  UseMethod("remove_outliers")
}

#' Remove outlier droplets
#' 
#' The algorithm for removing outlier droplets from a plate
#' 
#' @export
#' @keywords internal
remove_outliers.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step('REMOVE_OUTLIERS')
  plate %>% check_step(CURRENT_STEP, TRUE)
  step_begin("Finding outlier droplets")
  
  data <- plate_data(plate)
  
  # ---

  outlier_cutoff <- plate %>% get_outlier_cutoff
  cutoff_x <- outlier_cutoff[[x_var(plate)]]
  cutoff_y <- outlier_cutoff[[y_var(plate)]]

  CLUSTER_OUTLIER <- plate %>% cluster('OUTLIER')
  outlier_idx <-
    (data[[y_var(plate)]] > cutoff_y | data[[x_var(plate)]] > cutoff_x)
  data[outlier_idx, 'cluster'] <- CLUSTER_OUTLIER  
  
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