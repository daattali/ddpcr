analyze_well_clusters_normals <- function(plate, well_id, plot = FALSE) {
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
  
  cl_borders <- get_filled_borders(plate, well_id)
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
    mt_drops <- top %>% dplyr::filter_(~ HEX %btwn% mt_borders)      
    wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
    graphics::plot(well_data, "HEX", "FAM")
    points(top, col = "blue")
    points(mt_drops, col = "purple")
    points(wt_drops, col = "green")
    abline(h = cl_borders %>% mean, col = "grey")
    abline(h = cl_borders, col = "black")
    abline(v = mixmdl_hex$mu, col = "grey")
    abline(v = mt_borders, col = "purple")
    abline(v = wt_borders, col = "green")
    mt_freq <- signif(nrow(mt_drops) / (nrow(mt_drops) + nrow(wt_drops)) * 100, 3)
    title(sprintf("%s (%s%%)", well_id, mt_freq))
  }
  
  #   well_data[['cluster']] <- CLUSTER_RAIN
  #   mt_idx <- well_data[['FAM']] %btwn% cl_borders & well_data[['HEX']] %btwn% mt_borders
  #   well_data[mt_idx, 'cluster'] <- CLUSTER_MT
  #   wt_idx <- well_data[['FAM']] %btwn% cl_borders & well_data[['HEX']] %btwn% wt_borders
  #   well_data[wt_idx, 'cluster'] <- CLUSTER_WT
  
  return(list(#result = well_data,
    mt_borders = mt_borders %>% border_to_str,
    wt_borders = wt_borders %>% border_to_str,
    cl_borders = cl_borders %>% border_to_str,
    has_mt_cluster = has_mt_cluster,
    comment = msg))
}