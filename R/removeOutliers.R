# Removes outlier drops from a plate
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   Dataframe with outliers removed
removeOutliers <- function() {
  # TODO(daattali) ensure status == loaded
  if (private$debug) {
    tstart <- proc.time()
  }  
  
  outlierInfo <- private$getOutlierCutoff()
  cutoffHEX <- outlierInfo$HEX
  cutoffFAM <- outlierInfo$FAM
  
  outliers <-
    private$plateData %>%
    dplyr::select_("well", "HEX", "FAM") %>%
    dplyr::filter_(lazyeval::interp(~ FAM > cutoffFAM | HEX > cutoffHEX,
                                    FAM = quote(FAM), HEX = quote(HEX)))
  private$outliers <- outliers

  private$plateData %<>% 
    dplyr::filter_(lazyeval::interp(~ !(FAM > cutoffFAM | HEX > cutoffHEX),
                                    FAM = quote(FAM), HEX = quote(HEX)))
  
  private$plateMeta <-
    private$plateData %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops" = ~ n()) %>%
    dplyr::left_join(private$plateMeta, ., by = "well")
  
  private$status <- STATUS_OUTLIERS_REMOVED
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to remove outliers (s): %s (%s drops identified as outliers)",
                    round(tend-tstart)[1], nrow(private$outliers)))
  }  

  invisible(self)
}

getOutliers <- function() {
  private$outliers
}


# Determines the cutoffs for outliers on the plate.
# Note this function is currently only being used on the whole plate and
# thus determines outliers on a plate-level, but it can also be used to find
# outliers in each well by passing only data from a single well
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   list:
#     HEX: the HEX value of the outlier cutoff
#     FAM: the FAM value of the outlier cutoff
#
# Algorithm:
#   The idea borrows from outlier detection in normal populations (looking for
#   points that are further than k*IQR from the 1st/3rd quartiles), but since
#   our data is highly skewed and non-normal, I use a small tweak.
#   For each dimension ([FAM, HEX]): get the 1% (PARAMS$OUTLIERS$TOP_PERCENT) of
#   drops that have the highest value in that dimension.  Calculate the IQR of the
#   values only within these drops. Mark the outlier cutoff as the 3rd quantile
#   plus 5 (PARAMS$OUTLIERS$CUTOFF_IQR) IQR
getOutlierCutoff <- function() {
  topFAM <- head(sort(private$plateData$FAM, decreasing=TRUE), nrow(private$plateData)/100*PARAMS$OUTLIERS$TOP_PERCENT)
  qFAM <- quantile(topFAM, c(.25, .75))
  cutoffFAM <- as.numeric((qFAM[2] - qFAM[1]) * PARAMS$OUTLIERS$CUTOFF_IQR + qFAM[2])
  
  topHEX <- head(sort(private$plateData$HEX, decreasing=TRUE), nrow(private$plateData)/100*PARAMS$OUTLIERS$TOP_PERCENT)
  qHEX <- quantile(topHEX, c(.25, .75))
  cutoffHEX <- as.numeric((qHEX[2] - qHEX[1]) * PARAMS$OUTLIERS$CUTOFF_IQR + qHEX[2])
  
  result <- list(HEX = cutoffHEX, FAM = cutoffFAM)
  
  result
}

Plate$set("public", "removeOutliers", removeOutliers)
Plate$set("public", "getOutliers", getOutliers)
Plate$set("private", "getOutlierCutoff", getOutlierCutoff)
Plate$set("private", "outliers", NULL)