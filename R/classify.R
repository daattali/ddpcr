analyze_well_clusters <- function(well_id, plot = FALSE) {
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
    )
  
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
    )
  wt_borders <-
    plus_minus(
      mixmdl_hex$mu[larger_comp_hex],
      mixmdl_hex$sigma[larger_comp_hex] *
        params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
    )
  
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
        )
      wt_borders <-
        plus_minus(
          mixmdl_hex$mu[larger_comp_hex],
          mixmdl_hex$sigma[larger_comp_hex] *
            params[['ASSIGN_CLUSTERS']][['CLUSTERS_BORDERS_NUM_SD']]
        )
    }
  } else {
    mt_cutoff <-
      mixmdl_hex$mu[larger_comp_hex] -
      mixmdl_hex$sigma[larger_comp_hex] * 
        params[['ASSIGN_CLUSTERS']][['NO_CLUSTER_MT_BORDER_NUM_SD']]
    mt_borders <- c(0, mt_cutoff)
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
  
  return(list(result = well_data,
              has_mt_cluster = has_mt_cluster,
              comment = msg))
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
  
  clusters_data <- analysis_methods(well_id, plot)
  clusters_well_data <- clusters_data[['result']]
  
  well_data <- 
    get_single_well(plate, well_id, full = TRUE, clusters = FALSE) %>%
    dplyr::mutate(cluster = CLUSTER_EMPTY) %>%
    dplyr::left_join(clustersWellData, by = c("HEX","FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y)
  
  clustersData[['result']] <- wellDataSingle
  clustersData
}

analyze_well_clusters_density <- function(well_id, plot = FALSE) {
  
}



#' @export
classify_droplets <- function(plate, analysisMethodIdx = 2) {
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
  
  wellsInfo <-
    lapply(private$wellsSuccess(), function(x) {
      res <- self$classifyDropletsSingle(x, analysisMethodIdx)
      res[['result']][['well']] <- x
      res[['well']] <- x
      res
    })
  
  # combine all the dataframes into one big dataframe containing all the drops
  # from successful wells with their associated clusters
  result <- plyr::ldply(wellsInfo, function(x) x$result)
  # add metadata (comment/hasMTclust) to each well
  clustersMetadata <- plyr::ldply(wellsInfo, function(x) {
    x$result <- NULL
    data.frame(x, stringsAsFactors = F)
  })
  
  # add the data from unsuccessful wells
  failedWells <- private$wellsFailed()
  failedWellsData <-
    self$getData() %>%
    dplyr::filter(well %in% failedWells) %>%
    dplyr::select(well, HEX, FAM) %>%
    dplyr::mutate(cluster = CLUSTER_UNDEFINED)
  result <- rbind(result, failedWellsData)
  
  newMetadata <-
    private$plateMeta %>%
    dplyr::left_join(clustersMetadata, by = "well") %>%
    dplyr::mutate(comment = ifelse(is.na(comment.y), comment.x, comment.y)) %>%
    dplyr::select(-comment.x, -comment.y)
  
  private$plateMeta <- newMetadata
  private$plateData <- result
  private$status <- STATUS_DROPLETS_CLASSIFIED
  
  private$calculateMtFreqs()
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to analyze droplets to find mutant/wild-type drops (s): %s",
                    round(tend-tstart)[1]))
  }
  
  plate
}