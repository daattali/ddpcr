# For a given well, calculate the FAM cutoff where all drops below this
# value are considered empty
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .plot: If true, plot the result (used mainly for development/debugging)
#
# Returns:
#   The FAM value that is the cutoff for empty drops in the well
#
# Algorithm:
#   We fit a mixture of 2 normal populations in the FAM values of all the drops.
#   The lower population corresponds to the empty drops, which form a fairly
#   dense cluster. To capture the cutoff of empty drops, we simply assume that
#   the FAM intensity of empty drops can be roughly modeled by a normal distribution.
#   More specifically, we set the threshold at 7 (PARAMS$EMPTY$CUTOFF_SD)
#   standard deviations above the center of the distribution.
get_empty_cutoff <- function(plate, well_id) {
  params <- params(plate)
  
  well_data <- get_single_well(plate, well_id, full = TRUE, clusters = FALSE)
  
  # fit two normal distributions in the data along the FAM dimension
  set.seed(SEED)
  quiet(
    mixmdl_fam <- mixtools::normalmixEM(well_data[['FAM']], k = 2))
  
  # set the FAM cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_fam <- mixmdl_fam$mu %>% which.min
  cutoff_fam <-
    (mixmdl_fam$mu[smaller_comp_fam] +
    params[['EMPTY']][['CUTOFF_SD']] * mixmdl_fam$sigma[smaller_comp_fam]) %>%
    ceiling %>%
    as.integer
  
  cutoff_fam
}


# Find the empty drops on the plate and mark them with the empty cluster
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .metadata: The dataframe containing the metadata of plate
#
# Returns:
#   list:
#     result: Dataframe containing all drops, with the empty drops marked with
#       with their cluster
#     cutoffs: The FAM cutoff of every well
remove_empty <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_FAILED_REMOVED)
  
  tstart <- proc.time()
  
  # ---
  
  # get the empty cutoff of every well
  empty_cutoff_map <-
    vapply(wells_success(plate),
           function(x) get_empty_cutoff(plate, x),
           integer(1))
  
  # set the cluster to EMPTY for every empty droplet in every well
  data <- plate_data(plate)
  lapply(names(empty_cutoff_map),
         function(well_id){
           cutoff <- empty_cutoff_map[well_id] %>% as.numeric
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           empty_idx <- data[['well']] == well_id & data[['FAM']] < cutoff
           
           # this is a bit ugly but it's much faster to keep overwriting the
           # data rather than create many small dataframes and then merging/
           # overwriting with the original data
           data[empty_idx, 'cluster'] <- CLUSTER_EMPTY
           assign("data", data, envir = parent.env(environment()))
           
           NULL
         }
  ) %>% invisible
  
  meta <-
    data %>%
    dplyr::filter_(~ cluster == CLUSTER_EMPTY) %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops_empty" = ~ n()) %>%
    merge_dfs_overwrite_col(plate_meta(plate), ., "drops_empty") %>%
    dplyr::mutate_(
      "drops_non_empty" = ifelse(is.na("drops_empty"),
                                 NA,
                                 "drops - drops_empty"),
      "drops_empty_fraction" = ifelse(is.na("drops_empty"),
                                      NA,
                                      "signif(drops_empty / drops, 3)"))
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_concentration
  
  status(plate) <- STATUS_EMPTY_REMOVED
  
  tend <- proc.time()
  message(sprintf("Time to find empty droplets: %s seconds",
                  round(tend-tstart)[1]))
  
  plate
}

