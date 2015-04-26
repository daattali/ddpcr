#' @export
reclassify_droplets_single <- function(plate, well_id, ...) {
  UseMethod("reclassify_droplets_single")
}

#' @export
reclassify_droplets_single.ppnp_assay <- function(plate, well_id, consensus_border_ratio) {
  
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)
  
  filled <-
    plate %>%
    well_info(well_id, 'filled_borders') %>%
    str_to_border %>%
    {dplyr::filter_(well_data,
                    lazyeval::interp(~ var %btwn% .,
                                     var = as.name(positive_var)))}
  
  positive_median <- 
    well_data %>%
    dplyr::filter_(~ cluster == CLUSTER_POSITIVE) %>%
    .[[variable_var]] %>%
    median    
  
  negative_cutoff <- (consensus_border_ratio * positive_median) %>% as.integer
  negative_borders <- c(0, negative_cutoff)
  positive_borders <- c(negative_borders[2] + 1, filled[[variable_var]] %>% max)
  
  # TODO recalculate whehter or not has_mt_cluster is true
  
  return(list(
    negative_borders = negative_borders %>% border_to_str,
    positive_borders = positive_borders %>% border_to_str
  ))
}

#' @export
reclassify_droplets <- function(plate) {
  UseMethod("reclassify_droplets")
}
  
#' @export
reclassify_droplets.ppnp_assay <- function(plate) {
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
  
  data <- plate_data(plate)
  
  # ---
  
  # if there are not enough wells with high MT freq to use as prior info or if
  # there are no wells with low MT freq to reclassify, do nothing
  min_wells <- params(plate, 'RECLASSIFY_WELLS', 'MIN_WELLS_NEGATIVE_CLUSTER')
  if (plate %>% wells_negative %>% length < min_wells ||
      plate %>% wells_positive %>% length == 0) {
    message(paste0("Not reclassifying droplets because there are not enough",
                   " wells with significant ",
                   params(plate, 'GENERAL', 'NEGATIVE_NAME'),
                   " clusters"))
    return(plate)
  }
  
  # calculate the ratio of the MT border over highest WT drop (in HEX)
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
    quantile(params(plate, 'RECLASSIFY_WELLS', 'BORDER_RATIO_QUANTILE')) %>%
    as.numeric

  well_clusters_info <-
    vapply(plate %>% wells_positive,
           function(x) reclassify_droplets_single(plate, x, consensus_border_ratio),
           vector("list", 2)) %>%
    lol_to_df  
  
  # reclassify mutant drops for every well without a mutant drops cluster
  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells_positive(plate),
         function(well_id){
           well_info <-
             well_clusters_info %>%
             dplyr::filter_(~ well == well_id)
           
           filled_borders <- well_info(well_id, 'filled_borders') %>% str_to_border
           negative_borders <- well_info[['negative_borders']] %>% str_to_border
           positive_borders <- well_info[['positive_borders']] %>% str_to_border
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           # this code is a bit ugly but it's much faster to keep overwriting
           # the data rather than create many small dataframes to merge
           classifiable_idx <-
             data[['well']] == well_id &
             (data[['cluster']] == CLUSTER_UNDEFINED |
                data[['cluster']] >= CLUSTER_RAIN)
           filled_idx <- classifiable_idx & data[[positive_var]] %btwn% filled_borders
           negative_idx <- filled_idx & data[[variable_var]] %btwn% negative_borders
           positive_idx <- filled_idx & data[[variable_var]] %btwn% positive_borders
           
           data[classifiable_idx, 'cluster'] <- CLUSTER_RAIN
           data[negative_idx, 'cluster'] <- CLUSTER_NEGATIVE
           data[positive_idx, 'cluster'] <- CLUSTER_POSITIVE
           assign("data", data, envir = data_env)
           
           NULL
         }
  ) %>% invisible  
  
  # add metadata (comment/hasMTclust) to each well
  meta <-
    plate_meta(plate) %>%
    merge_dfs_overwrite_col(well_clusters_info)
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_mt_freqs
  
  status(plate) <- STATUS_DROPLETS_RECLASSIFIED
  
  tend <- proc.time()
  message(paste("Time to reclassify droplets based on info in all wells ",
                round(tend-tstart)[1], "seconds"))

  plate
}