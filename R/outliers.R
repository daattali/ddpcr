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
  
  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR')  
  top_y <- 
    sort(data[[Y_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_y <- quantile(top_y, c(.25, .75))
  cutoff_y <-
    (diff(q_y) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_y[2]) %>%
    as.numeric
  
  top_x <- 
    sort(data[[X_var]], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_x <- quantile(top_x, c(.25, .75))
  cutoff_x <-
    (diff(q_x) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_x[2]) %>%
    as.numeric
  
  result <- list()
  result[[X_var]] <- cutoff_x
  result[[Y_var]] <- cutoff_y
  
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
  stopifnot(plate %>% status >= STATUS_FAILED_REMOVED)
  
  tstart <- proc.time()
  
  data <- plate_data(plate)
  
  # ---

  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR')    
  outlier_cutoff <- plate %>% get_outlier_cutoff
  cutoff_x <- outlier_cutoff[[X_var]]
  cutoff_y <- outlier_cutoff[[Y_var]]

  outlier_idx <-
    (data[[Y_var]] > cutoff_y | data[[X_var]] > cutoff_x)
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
  status(plate) <- STATUS_OUTLIERS_REMOVED
  
  tend <- proc.time()
  message(sprintf("Time to remove outliers: %s seconds",
                  round(tend-tstart)[1]))

  plate
}