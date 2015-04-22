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
get_outlier_cutoff <- function(plate) {
  data <-
    plate_data(plate) %>%
    dplyr::filter_(~ well %in% wells_success(plate))
  
  top_fam <- 
    sort(data[['FAM']], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_fam <- quantile(top_fam, c(.25, .75))
  cutoff_fam <-
    (diff(q_fam) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_fam[2]) %>%
    as.numeric
  
  top_hex <- 
    sort(data[['HEX']], decreasing = TRUE) %>%
    head(nrow(data) / 100 * params(plate, 'OUTLIERS', 'TOP_PERCENT'))
  q_hex <- quantile(top_hex, c(.25, .75))
  cutoff_hex <-
    (diff(q_hex) * params(plate, 'OUTLIERS', 'CUTOFF_IQR') + q_hex[2]) %>%
    as.numeric
  
  result <- list()
  result[['HEX']] <- cutoff_hex
  result[['FAM']] <- cutoff_fam
  
  result
}

# Removes outlier drops from a plate
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   Dataframe with outliers removed
#' @export
remove_outliers <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_FAILED_REMOVED)
  
  tstart <- proc.time()
  
  data <- plate_data(plate)
  
  # ---
  
  outlier_cutoff <- plate %>% get_outlier_cutoff
  cutoff_hex <- outlier_cutoff[['HEX']]
  cutoff_fam <- outlier_cutoff[['FAM']]

  outlier_idx <-
    (data[['FAM']] > cutoff_fam | data[['HEX']] > cutoff_hex)
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