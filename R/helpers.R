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
get_single_well <- function(plate, well_id, full = FALSE, clusters = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  result <-
    plate_data(plate) %>%
    dplyr::filter_(lazyeval::interp(
      ~ well == well_id,
      well = quote(well))) %>%
    dplyr::select_(quote(-well))
  
  if (!full) {
    result %<>%
      dplyr::filter_(lazyeval::interp(
        ~ cluster != CLUSTER_EMPTY,
        cluster = quote(cluster)))
  }
  
  if (!clusters) {
    result %<>%
      dplyr::select_(quote(-cluster))
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

get_filled_borders <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id)
  params <- params(plate)
  set.seed(SEED)
  quiet(
    mixmdl_fam <- mixtools::normalmixEM(well_data[['FAM']], k = 2))
  larger_comp_fam <- mixmdl_fam$mu %>% which.max
  cl_borders <-
    plus_minus(
      mixmdl_fam$mu[larger_comp_fam],
      mixmdl_fam$sigma[larger_comp_fam] *
        params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
    ) %>%
    as.integer
  
  cl_borders
}