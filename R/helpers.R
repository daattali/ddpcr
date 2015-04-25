# Retrieve droplet data for a single well.
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .full: If true, get all droplets, including the empty ones
#   .clusters: If true, return information about which cluster each drop belongs to
#
# Returns:
#   Dataframe containing only droplet data for the given well
get_single_well <- function(plate, well_id,
                            empty = FALSE, outliers = FALSE, clusters = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  well_id %<>% toupper
  
  result <-
    plate_data(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    dplyr::select_(quote(-well))
  
  if (!empty) {
    result %<>%
      dplyr::filter_(~ cluster != CLUSTER_EMPTY)
  }
  if (!outliers) {
    result %<>%
      dplyr::filter_(~ cluster != CLUSTER_OUTLIER)
  }
  if (!clusters) {
    result %<>%
      dplyr::select_(~ -cluster)
  }
  
  result
}



calculate_mt_freq_single <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  mt_num <- (well_data[['cluster']] == CLUSTER_MT) %>% sum
  wt_num <- (well_data[['cluster']] == CLUSTER_WT) %>% sum
  mt_freq <- (mt_num / (mt_num + wt_num) * 100) %>% signif(3)
  
  list(mt_num = mt_num,
       wt_num = wt_num,
       mt_freq = mt_freq)
}

calculate_mt_freqs <- function(plate) {
  mt_freqs <-
    vapply(wells_success(plate),
           function(x) calculate_mt_freq_single(plate, x),
           list('mt_num', 'wt_num', 'mt_freq')) %>%
    lol_to_df
  
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(mt_freqs, setdiff(names(mt_freqs), "well"))
  
  plate
}

