classifyDroplets <- function(analysisMethodIdx = 2) {
  #TODO(daattali) use SE in this whole function
  # Mark all drops in a plate with their corresponding clusters, including
  # undefined clusters for failed wells
  #
  # Side effects:
  
  # get a list containing, for each successful well:
  # - a dataframe with all the drops with their clusters
  # - whether or not there is a mutant drops cluster
  # - any comment by the algorithm
  
  tstart <- proc.time()
  
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
  
  invisible(self)
}

classifyDropletsSingle <- function(wellNum, analysisMethodIdx = 2, plot = FALSE) {
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
  
  analysisMethods <- c(private$analyzeWellClusters, private$analyzeWellClustersDensity)
  analysisMethod <- analysisMethods[[analysisMethodIdx]]

  clustersData <- analysisMethod(wellNum, plot)
  clustersWellData <- clustersData[['result']]
  
  wellDataSingle <-
    private$getSingleWell(wellNum, full = TRUE) %>%
    dplyr::mutate(cluster = CLUSTER_EMPTY) %>%
    dplyr::left_join(clustersWellData, by = c("HEX","FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y)
  
  clustersData[['result']] <- wellDataSingle
  clustersData
}

analyzeWellClusters <- function(wellNum, plot = FALSE) {
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
  
  
  hasMTcluster <- FALSE
  msg <- NA
  wellDataSingle <- private$getSingleWell(wellNum)
  
  set.seed(SEED)
  quiet(
    mixmdlF <- mixtools::normalmixEM(wellDataSingle$FAM, k = 2))
  largerComp <- which.max(mixmdlF$mu)
  clborders <- mixmdlF$mu[largerComp] +
    mixmdlF$sigma[largerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)
  
  top <- subset(wellDataSingle, FAM > clborders[1] & FAM < clborders[2])

  set.seed(SEED)
  for (i in seq(PARAMS$ASSIGN_CLUSTERS$NUM_ATTEMPTS_SEGREGATE)) {
    quiet(
      mixmdlH <- mixtools::normalmixEM(top$HEX, k = 2))
    if (min(mixmdlH$mu) < max(mixmdlH$mu) * PARAMS$ASSIGN_CLUSTERS$SEGREGATE_RATIO_THRESHOLD) {
      hasMTcluster <- TRUE
      break
    }
  }
  
  smallerComp <- which.min(mixmdlH$mu)
  largerComp <- which.max(mixmdlH$mu)
  mtborders <- mixmdlH$mu[smallerComp] +
    mixmdlH$sigma[smallerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)
  wtborders <- mixmdlH$mu[largerComp] +
    mixmdlH$sigma[largerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)  
  
  set.seed(SEED)
  if (hasMTcluster) {
    # if the rain in the HEX is too strong, it's possible to either have overlapping
    # borders or have the mutant cluster being wider than the wildtype cluster
    # (which shouldn't happen), so in both cases, we just fit three gaussians
    # instead of 2, and use the right and left ones are the wild-type/mutant clusters
    if (wtborders[1] < mtborders[2]) {
      msg <- "using 3 HEX gaussians to find clusters (MT and WT cluster borders overlapped)"
    } else if (mixmdlH$sigma[smallerComp] > mixmdlH$sigma[largerComp]) {
      msg <- "using 3 HEX gaussians to find clusters (SD of MT cluster > SD of WT cluster)"
    }
    
    if (!is.na(msg)) {
      quiet(
        mixmdlH <- mixtools::normalmixEM(top$HEX, k = 3))
      smallerComp <- which.min(mixmdlH$mu)
      largerComp <- which.max(mixmdlH$mu)
      mtborders <- mixmdlH$mu[smallerComp] +
        mixmdlH$sigma[smallerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)
      wtborders <- mixmdlH$mu[largerComp] +
        mixmdlH$sigma[largerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)
    }
  } else {
    mtcutoff <- mixmdlH$mu[largerComp] -
      mixmdlH$sigma[largerComp]*PARAMS$ASSIGN_CLUSTERS$NO_CLUSTER_MT_BORDER_NUM_SD
    mtborders <- c(0, mtcutoff)
  }
  
  # TODO(daattali) use SE
  mt <- subset(top, HEX > mtborders[1] & HEX < mtborders[2])
  wt <- subset(top, HEX > wtborders[1] & HEX < wtborders[2])
  
  if(plot){
    graphics::plot(wellDataSingle, "HEX", "FAM")
    points(top, col = "blue")
    points(mt, col = "purple")
    points(wt, col = "green")
    abline(h = mixmdlF$mu, col = "grey")
    abline(h = clborders, col = "black")
    abline(v = mixmdlH$mu, col = "grey")
    abline(v = mtborders, col = "purple")
    abline(v = wtborders, col = "green")
    mtFreq <- signif(nrow(mt) / (nrow(mt) + nrow(wt)) * 100, 3)
    title(sprintf("%s (%s%%)", wellNum, mtFreq))
  }
  
  # store the cluster on each droplet and return the result
  #TODO(aattali) use SE
  wt <- dplyr::mutate(wt, cluster = CLUSTER_WT)
  mt <- dplyr::mutate(mt, cluster = CLUSTER_MT)
  wellDataSingle %<>%
    dplyr::mutate(cluster = CLUSTER_RAIN) %>%
    dplyr::left_join(wt, by = c("HEX", "FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y) %>%
    dplyr::left_join(mt, by = c("HEX", "FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y)
  
  return(list(result = wellDataSingle,
              hasMTcluster = hasMTcluster,
              comment = msg))
}

Plate$set("public", "classifyDroplets", classifyDroplets)
Plate$set("public", "classifyDropletsSingle", classifyDropletsSingle)
Plate$set("private", "analyzeWellClusters", analyzeWellClusters)