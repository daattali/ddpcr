reclassify_droplets_single <- function(plate, well_id, consensus_border_ratio) {
  params <- params(plate)
  
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  top <-
    plate %>%
    well_info(well_id, 'cl_borders') %>%
    str_to_border %>%
    {dplyr::filter_(well_data, ~ FAM %btwn% .)}

  wt_median <- 
    well_data %>%
    dplyr::filter_(~ cluster == CLUSTER_WT) %>%
    .[['HEX']] %>%
    median    
  
  mt_cutoff <- (consensus_border_ratio * wt_median) %>% as.integer
  mt_borders <- c(0, mt_cutoff)
  wt_borders <- c(mt_borders[2] + 1, max(top[['HEX']]))
  
  # TODO recalculate whehter or not has_mt_cluster is true
  # TODO rename has_mt_clustr to ~is_wildtype
  
  return(list(
    mt_borders = mt_borders %>% border_to_str,
    wt_borders = wt_borders %>% border_to_str))
}

#' @export
reclassify_droplets <- function(plate) {
  # Reclassify mutant drops in wells with low mutant frequency.  The initial
  # classification was done more naively, but after analyzing all the wells,
  # we can now use the high mutant frequency wells as a reference and try to
  # be smarter about where the mutant drops should be.
  #
  # Args:
  #   wellData: The dataframe containing all the droplets
  #   metadata: The dataframe containing the metadata of plate
  #
  # Returns:
  #   list:
  #     success: TRUE if enough information was available to reclassify low MT wells,
  #       FALSE otherwise
  #     result: Dataframe containing all the drops and their assigned clusters,
  #        with the mutant drops in the low MT freq wells reassigned 
  #
  # Algorithm:
  #   wells without a significant mutant cluster have very few (if any) mutant
  #   drops, so it's difficult and highly variable to identify them.  We can try
  #   to leverage data from wells with many mutant drops to get a good idea of
  #   where mutant drops should be found in a low mutant frequency well.  However,
  #   this is only possible if there is enough data to learn from - ie. if there are 
  #   less than 4 (PARAMS$RECLASSIFY_LOW_MT$MIN_WELLS_MT_CLUSTER) wells with 
  #   high mutant frequency, then we skip this step.  
  #   For every well with a high mutant frequency, we want to know where the
  #   mutant cluster is relative to the wild-type cluster. To do this, 
  #   we look at where the mutant cluster right-most (HEX) border is and
  #   where the right-most wild-type drop is for every high mutant frequency well.
  #   We calculate the ratio of the mutant border HEX over the largest wild-type HEX,
  #   and call this the mutant-to-wildtype-border ratio.  In the wild-type cluster,
  #   we decide to measure the highest HEX value of a drop instead of the actual border
  #   because many times the right border is outside the range of the plot and is
  #   less informative.
  #   After calculating this ratio for all high mutant frequency wells, we
  #   choose a single value to use as a "consensus" mutant-to-wildtype-border-ratio.
  #   Instead of taking the mean or median, we take the the 3rd quartile
  #   (PARAMS$RECLASSIFY_LOW_MT$BORDER_RATIO_QUANTILE) in order to be more
  #   sensitive and not lose too many mutant drops.
  #   After obtaining the consensus mutant-to-wildtype-border-ratio, we can
  #   look at every low mutant frequency well, and based on the largest wild-type
  #   HEX in each well, we can use the ratio to determine where to draw the mutant
  #   cluster border.  In the FAM dimension, we make the FAM borders for
  #   for the mutant cluster the same as the borders for the wild-type cluster.
  #   With these new mutant cluster borders in place, we first reclassify all 
  #   the previously assigned mutant drops as rain, and then assign the mutant
  #   label to any drop that falls inside the new borders.
  
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_DROPLETS_CLASSIFIED)
  
  tstart <- proc.time()
  
  params <- params(plate)
  data <- plate_data(plate)
  
  # ---
  
  # if there are not enough wells with high MT freq to use as prior info or if
  # there are no wells with low MT freq to reclassify, do nothing
  if (plate %>% wells_mutant %>% length < params[['RECLASSIFY_LOW_MT']][['MIN_WELLS_MT_CLUSTER']]
      | plate %>% wells_wildtype %>% length == 0) {
    message(paste0("Reclassifying droplets in low mutant frequency wells did not take place",
                   "because there are noth enough high mutant frequency wells."))
    return(plate)
  }
  
  # calculate the ratio of the MT border over highest WT drop (in HEX)
  mt_border_ratios <-
    vapply(plate %>% wells_mutant,
           function(x) {
             mt_max <- 
               data %>%
               dplyr::filter_(~ well == x,
                              ~ cluster == CLUSTER_MT) %>%
               .[['HEX']] %>%
               max
             wt_median <- 
               data %>%
               dplyr::filter_(~ well == x,
                              ~ cluster == CLUSTER_WT) %>%
               .[['HEX']] %>%
               median               
             ratio <- mt_max / wt_median
             ratio
           },
           numeric(1))
  consensus_border_ratio <-
    mt_border_ratios %>%
    quantile(params[['RECLASSIFY_LOW_MT']][['BORDER_RATIO_QUANTILE']]) %>%
    as.numeric

  well_clusters_info <-
    vapply(wells_wildtype(plate),
           function(x) reclassify_droplets_single(plate, x, consensus_border_ratio),
           list('mt_borders', 'wt_borders')) %>%
    lol_to_df  
  
  # reclassify mutant drops for every well without a mutant drops cluster
  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells_wildtype(plate),
         function(well_id){
           well_info <-
             well_clusters_info %>%
             dplyr::filter_(~ well == well_id)
           cl_borders <- well_info(plate, well_id, 'cl_borders') %>% str_to_border
           mt_borders <- well_info[['mt_borders']] %>% str_to_border
           wt_borders <- well_info[['wt_borders']] %>% str_to_border
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           # this code is a bit ugly but it's much faster to keep overwriting
           # the data rather than create many small dataframes to merge
           non_empty_idx <-
             data[['well']] == well_id &
             data[['cluster']] >= CLUSTER_WT
           cl_idx <- data[['well']] == well_id & data[['FAM']] %btwn% cl_borders
           mt_idx <- cl_idx & data[['well']] == well_id & data[['HEX']] %btwn% mt_borders
           wt_idx <- cl_idx & data[['well']] == well_id & data[['HEX']] %btwn% wt_borders
           data[non_empty_idx, 'cluster'] <- CLUSTER_RAIN
           data[mt_idx, 'cluster'] <- CLUSTER_MT
           data[wt_idx, 'cluster'] <- CLUSTER_WT
           assign("data", data, envir = data_env)
           
           NULL
         }
  ) %>% invisible  
  
  # add metadata (comment/hasMTclust) to each well
  meta <-
    plate_meta(plate) %>%
    merge_dfs_overwrite_col(well_clusters_info,
                            setdiff(names(well_clusters_info), "well"))
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_mt_freqs
  
  status(plate) <- STATUS_DROPLETS_RECLASSIFIED
  
  tend <- proc.time()
  message(paste("Time to recalculate mutant droplets in low MT freq wells based",
                 "on high MT freq wells:", round(tend-tstart)[1], "seconds"))

  plate
}