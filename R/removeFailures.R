removeFailures <- function() {
  if (private$debug) {
    tstart <- proc.time()
  }  
  
  wellSuccessMap <- vapply(self$getMeta(TRUE)$well,
                           private$isWellSuccess,
                           list(logical(1), character(1)))
  
  wellSuccessMap %<>% t %>% as.data.frame
  wellSuccessMap$well <- row.names(wellSuccessMap)
  wellSuccessMap <- dplyr::rename(wellSuccessMap, success = result)
  wellSuccessMap$comment <- unlist(wellSuccessMap$comment)
  wellSuccessMap$success <- unlist(wellSuccessMap$success)

  private$plateMeta %<>%
    dplyr::left_join(wellSuccessMap, by = "well")
  
  private$status <- STATUS_FAILED_REMOVED
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to find unsuccessful wells (s): %s",
                    round(tend-tstart)[1]))
  }
  
  invisible(self)
}


# Determine if a well had a successful ddPCR run
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .plot: If true, plot the result (used mainly for development/debugging)
#
# Returns:
#   list:
#     result: TRUE if the well had a successful run, FALSE otherwise
#     comment: the reason the well was deemed unsuccessful, or NA for successful wells
#
# Algorithm:
#   The goal here is to see if there is anything that is clearly wrong with the
#   data in the well.  First, I ensure enough drops were loaded (BioRad claims 
#   20k, we see 12k-17k usually) - less than 5000 (PARAMS$WELLSUCCESS$TOTAL_DROPS_T)
#   is considered a failed run. Next I make a few basic sanity checks about the 
#   general look of the data in clusters.
#   If we were to normalize all HEX and FAM values to be between 0-1, we'd expect to
#   always have a very large cluster of empty drops near (0,0), a cluster of
#   wild-type drops near (1,1), and possibly a cluster of mutant drops neat (0,1).
#   Using these assumptions on the data, we make the following three heuristic
#   checks after fitting a mixture of 2 normal distributions in the FAM values
#   of all the drops:
#     - The centers (mu's) of the two populations need to be far enough (this
#       if checked by seeing if the larger center is at least twice the value of the
#       lower center.  If it is not, that usually indicates that both normal populations
#       are really the same cluster)
#     - The lower population cannot have too few drops (lambda < 0.3)
#       (PARAMS$WELLSUCCESS$NORMAL_LAMBDA_LOW_T), as that means there is no
#       empty cluster.  Likewise, there shouldn't be too many drops (lambda > 0.99)
#       (PARAMS$WELLSUCCESS$NORMAL_LAMBDA_HIGH_T), as that means there are not
#       enough drops with data
#     - The standard deviation (sigma) of the lower population should be fairly
#       small, below 200 (PARAMS$WELLSUCCESS$NORMAL_SIGMA_T), to ensure that
#       the empty cluster is indeed very dense as it should be
isWellSuccess <- function(wellNum) {

  wellDataSingle <- private$getSingleWell(wellNum, full = TRUE, clusters = FALSE)

  # First heuristic check: make sure there are enough droplets
  
  if (nrow(wellDataSingle) < PARAMS$WELLSUCCESS$TOTAL_DROPS_T) {
    success <- FALSE
    msg <- paste0("Not enough drops generated (", nrow(wellDataSingle), ")")
    return(list(result = success, comment = msg))
  }
  
  # fit two normal distributions in the data along the FAM dimension, check:
  # - the mu's of the two populations needs to be far enough
  # - bottom population needs to have lambda not too small and not too large
  # - sigma of bottom population should be fairly small
  set.seed(SEED)
  quiet(
    mixmdl <- mixtools::normalmixEM(wellDataSingle$FAM, k = 2))
  smallerComp <- which.min(mixmdl$mu)
  largerComp <- which.max(mixmdl$mu)
  
  if (mixmdl$mu[largerComp] - mixmdl$mu[smallerComp] < mixmdl$mu[smallerComp]) {
    success <- FALSE
    msg <- paste0("There seems to be mostly empty drops (mu's of FAM normals: ",
                  paste0(round(mixmdl$mu), collapse = " "), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl$lambda[smallerComp] < PARAMS$WELLSUCCESS$NORMAL_LAMBDA_LOW_T) {
    success <- FALSE
    msg <- paste0("Could not find significant empty cluster (lambda of FAM normal: ",
                  signif(mixmdl$lambda[smallerComp], 4), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl$lambda[smallerComp] > PARAMS$WELLSUCCESS$NORMAL_LAMBDA_HIGH_T) {
    success <- FALSE
    msg <- paste0("There are too many empty drops (lambda of FAM normal: ",
                  signif(mixmdl$lambda[smallerComp], 4), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl$sigma[smallerComp] > PARAMS$WELLSUCCESS$NORMAL_SIGMA_T) {
    success <- FALSE
    msg <- paste0("Could not find a dense empty cluster (sigma of FAM normal: ",
                  round(mixmdl$sigma[smallerComp]), ")")
    return(list(result = success, comment = msg))
  }    
  
  # if all the sanity checks passed, the run was successful
  return(list(result = TRUE, comment=NA))  
}

Plate$set("public", "removeFailures", removeFailures)
Plate$set("private", "isWellSuccess", isWellSuccess)