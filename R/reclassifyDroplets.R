reclassifyLowMt <- function() {
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
  
  tstart <- proc.time()
  
  metadata <- self$getMeta()
  wellData <- self$getData()
  
  # if there are not enough wells with high MT freq to use as prior info or if
  # there are no wells with low MT freq to reclassify, do nothing
  if (nrow(dplyr::filter(metadata, hasMTcluster)) < PARAMS$RECLASSIFY_LOW_MT$MIN_WELLS_MT_CLUSTER
      | nrow(dplyr::filter(metadata, !hasMTcluster)) == 0) {
    if (private$debug) {
      message(paste0("Reclassifying droplets in low mutant frequency wells did not take place",
                     "because there are noth enough high mutant frequency wells."))
    }
    return(invisible(self))
  }

  # calculate the ratio of the MT border over highest WT drop (in HEX)
  # TODO(daattali) - why do I have to declare clusteredWells as a variable and can't use it inline anymore?
  highMtWells <- dplyr::filter(metadata, hasMTcluster)$well
  mtBorderRatio <-
    vapply(highMtWells,
         function(x) {
           data <- private$getSingleWell(x, clusters = TRUE)
           ratio <-
             median(dplyr::filter(data, cluster == CLUSTER_WT)$HEX) /
             max(dplyr::filter(data, cluster == CLUSTER_MT)$HEX)
         },
         1)
  consensusBorderRatio <- as.numeric(
    quantile(mtBorderRatio, PARAMS$RECLASSIFY_LOW_MT$BORDER_RATIO_QUANTILE))
  
  # reclassify mutant drops for every well without a mutant drops cluster
  reclassified <- lapply(
    dplyr::filter(metadata, !hasMTcluster)$well, function(x) {
      data <- private$getSingleWell(x, clusters = TRUE)
      data$well <- x
      
      # calculate where the mutant cluster border is based on a consensus from
      # all the wells with significant MT clusters
      famCutoff <- min(dplyr::filter(data, cluster == CLUSTER_WT)$FAM)
      mtHexCutoff <- median(dplyr::filter(data, cluster == CLUSTER_WT)$HEX) / consensusBorderRatio
      mt <- data %>%
        dplyr::select(HEX, FAM) %>%
        dplyr::filter(FAM > famCutoff, HEX <= mtHexCutoff) %>%
        dplyr::mutate(cluster = CLUSTER_MT)
      wt <- data %>%
        dplyr::select(HEX, FAM) %>%
        dplyr::filter(FAM > famCutoff, HEX > mtHexCutoff) %>%
        dplyr::mutate(cluster = CLUSTER_WT)
      
      data %<>%
        dplyr::left_join(mt, by = c("HEX", "FAM")) %>%
        dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
        dplyr::select(-cluster.x, -cluster.y) %>%
        dplyr::left_join(wt, by = c("HEX", "FAM")) %>%
        dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
        dplyr::select(-cluster.x, -cluster.y)
      
      data
    }
  )
  
  reclassified %<>% dplyr::bind_rows()
  
  reclassified %<>%
    dplyr::right_join(wellData, by = c("well", "HEX", "FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.x), cluster.y, cluster.x)) %>%
    dplyr::select(-cluster.x, -cluster.y)
  
  private$plateData <- reclassified
  
  private$status <- STATUS_DROPLETS_RECLASSIFIED
  
  private$calculateMtFreqs()
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to recalculate mutant droplets in low MT freq wells based on high MT freq wells (s): %s",
                    round(tend-tstart)[1]))
  }
  
  invisible(self)
}

Plate$set("public", "reclassifyLowMt", reclassifyLowMt)
