analyze_well_clusters <- function(plate, well_id, plot = FALSE) {
  # Given a well with the empty drops marked as empty, analyze the rest of the
  # drops and assign each to a cluster (rain/mutant/wildtype)
  #
  # Args:
  #   wellNum: The id of the well of interest
  #   plot: If true, plot the result (used mainly for development/debugging)
  #
  # Returns:
  #   list:
  #     result: Dataframe with the non-empty drops of a given well assigned to clusters
  #     hasMTcluster: TRUE if a significant mutant cluster was found, FALSE otherwise.
  #       Note that TRUE can be very good proxy for saying the well has mutant BRAFV600,
  #       and FALSE is a proxy for saying the well has wild-type BRAFV600
  #     comment: any comment raised by the algorithm, or NA if everything ran smoothly
  #
  # Algorithm:
  #   
  
  params <- params(plate)
  
  has_mt_cluster <- FALSE
  msg <- NA
  well_data <- get_single_well(plate, well_id)
  
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
  
  top <-
    well_data %>%
    dplyr::filter_(~ FAM %btwn% cl_borders) 
  
  set.seed(SEED)
  for (i in seq(params[['ASSIGN_CLUSTERS']][['NUM_ATTEMPTS_SEGREGATE']])) {
    quiet(
      mixmdl_hex <- mixtools::normalmixEM(top[['HEX']], k = 2))
    segregate_ratio <- params[['ASSIGN_CLUSTERS']][['SEGREGATE_RATIO_THRESHOLD']]
    if (min(mixmdl_hex$mu) < max(mixmdl_hex$mu) * segregate_ratio) {
      has_mt_cluster <- TRUE
      break
    }
  }
  
  smaller_comp_hex <- mixmdl_hex$mu %>% which.min
  larger_comp_hex <- mixmdl_hex$mu %>% which.max
  mt_borders <-
    plus_minus(
      mixmdl_hex$mu[smaller_comp_hex],
      mixmdl_hex$sigma[smaller_comp_hex] *
        params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
    ) %>%
    as.integer
  wt_borders <-
    plus_minus(
      mixmdl_hex$mu[larger_comp_hex],
      mixmdl_hex$sigma[larger_comp_hex] *
        params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
    ) %>%
    as.integer
  
  set.seed(SEED)
  if (has_mt_cluster) {
    # if the rain in the HEX is too strong, it's possible to either have overlapping
    # borders or have the mutant cluster being wider than the wildtype cluster
    # (which shouldn't happen), so in both cases, we just fit three gaussians
    # instead of 2, and use the right and left ones are the wild-type/mutant clusters
    if (wt_borders[1] < mt_borders[2]) {
      msg <- "using 3 HEX gaussians to find clusters (MT and WT cluster borders overlapped)"
    } else if (mixmdl_hex$sigma[smaller_comp_hex] > mixmdl_hex$sigma[larger_comp_hex]) {
      msg <- "using 3 HEX gaussians to find clusters (SD of MT cluster > SD of WT cluster)"
    }
    
    if (!is.na(msg)) {
      quiet(
        mixmdl_hex <- mixtools::normalmixEM(top[['HEX']], k = 3))
      smaller_comp_hex <- mixmdl_hex$mu %>% which.min
      larger_comp_hex <- mixmdl_hex$mu %>% which.max
      mt_borders <-
        plus_minus(
          mixmdl_hex$mu[smaller_comp_hex],
          mixmdl_hex$sigma[smaller_comp_hex] *
            params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
        ) %>% 
        as.integer
      wt_borders <-
        plus_minus(
          mixmdl_hex$mu[larger_comp_hex],
          mixmdl_hex$sigma[larger_comp_hex] *
            params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
        ) %>%
        as.integer
    }
  } else {
    mt_cutoff <-
      mixmdl_hex$mu[larger_comp_hex] -
      mixmdl_hex$sigma[larger_comp_hex] * 
        params[['ASSIGN_CLUSTERS']][['NO_CLUSTER_MT_BORDER_NUM_SD']]
    mt_borders <- c(0, mt_cutoff) %>% as.integer
  }

  if (plot) {
    mt_drops <- 
      top %>%
      dplyr::filter_(~ HEX %btwn% mt_borders)
    wt_drops <-
      top %>%
      dplyr::filter_(~ HEX %btwn% wt_borders)
    graphics::plot(well_data, "HEX", "FAM")
    points(top, col = "blue")
    points(mt_drops, col = "purple")
    points(wt_drops, col = "green")
    abline(h = mixmdl_fam$mu, col = "grey")
    abline(h = cl_borders, col = "black")
    abline(v = mixmdl_hex$mu, col = "grey")
    abline(v = mt_borders, col = "purple")
    abline(v = wt_borders, col = "green")
    mt_freq <- signif(nrow(mt_drops) / (nrow(mt_drops) + nrow(wt_drops)) * 100, 3)
    title(sprintf("%s (%s%%)", well_id, mt_freq))
  }

  well_data[['cluster']] <- CLUSTER_RAIN
  mt_idx <- well_data[['FAM']] %btwn% cl_borders & well_data[['HEX']] %btwn% mt_borders
  well_data[mt_idx, 'cluster'] <- CLUSTER_MT
  wt_idx <- well_data[['FAM']] %btwn% cl_borders & well_data[['HEX']] %btwn% wt_borders
  well_data[wt_idx, 'cluster'] <- CLUSTER_WT
  
  return(list(#result = well_data,
              mt_borders = mt_borders %>% border_to_str,
              wt_borders = wt_borders %>% border_to_str,
              cl_borders = cl_borders %>% border_to_str,
              has_mt_cluster = has_mt_cluster,
              comment = msg))
}

border_to_str <- function(border) {
  paste(border, collapse = ",")
}
str_to_border <- function(str) {
  strsplit(str, ",") %>% unlist %>% as.integer
}

classify_droplets_single <- function(plate, well_id, analysis_method_idx = 2, plot = FALSE) {
  # For a given well, merge the empty drops with the rain/mutant/wildtype drops
  # to result in a data frame containing all drops in a well marked with a cluster.
  #
  # Args:
  #   plot: If true, plot the result (used mainly for development/debugging)
  #
  # Returns:
  #   list:
  #     result: Dataframe with all drops in the given well assigned to clusters
  #     hasMTcluster: TRUE if a significant mutant cluster was found, FALSE otherwise.
  #       Note that TRUE can be very good proxy for saying the well has mutant BRAFV600,
  #       and FALSE is a proxy for saying the well has wild-type BRAFV600
  #     comment: any comment raised by the algorithm, or NA if everything ran smoothly 
  analysis_methods <- c(analyze_well_clusters, analyze_well_clusters_density)
  analysis_method <- analysis_methods[[analysis_method_idx]]
  clusters_data <- analysis_method(plate, well_id, plot)
  clusters_data
}

analyze_well_clusters_density <- function(well_id, plot = FALSE) {
  
}



#' @export
classify_droplets <- function(plate, analysis_method_idx = 2) {
  # Mark all drops in a plate with their corresponding clusters, including
  # undefined clusters for failed wells
  #
  # Side effects:
  
  # get a list containing, for each successful well:
  # - a dataframe with all the drops with their clusters
  # - whether or not there is a mutant drops cluster
  # - any comment by the algorithm
  
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_EMPTY_REMOVED)
  
  tstart <- proc.time()
  
  # ---
  
  well_clusters_info <-
    vapply(wells_success(plate),
           function(x) classify_droplets_single(plate, x, analysis_method_idx),
           list('mt_borders', 'wt_borders', 'cl_borders', 'has_mt_cluster', 'comment')) %>%
    lol_to_df

  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells_success(plate),
         function(well_id){
           well_info <-
             well_clusters_info %>%
             dplyr::filter_(~ well == well_id)
           cl_borders <- well_info[['cl_borders']] %>% str_to_border
           mt_borders <- well_info[['mt_borders']] %>% str_to_border
           wt_borders <- well_info[['wt_borders']] %>% str_to_border
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           # this code is a bit ugly but it's much faster to keep overwriting
           # the data rather than create many small dataframes to merge
           non_empty_idx <- data[['well']] == well_id &
             (data[['cluster']] == CLUSTER_UNDEFINED | data[['cluster']] >= CLUSTER_WT)
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
  
  status(plate) <- STATUS_DROPLETS_CLASSIFIED
  
  tend <- proc.time()
  message(sprintf("Time to classify droplet clusters: %s seconds",
                  round(tend-tstart)[1]))
  
  plate
}