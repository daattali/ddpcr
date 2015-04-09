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
markEmpty <- function() {
  tstart <- proc.time()
  
  # get the empty cutoff of every well
  emptyCutoffs <- vapply(private$wellsSuccess(),
                         private$getEmptyCutoff,
                         1L)
                         
  # make a dataframe with all the empty drops from every well
  quiet(
    lapply(names(emptyCutoffs), function(wellNum){
      private$plateData[private$plateData[['well']] == wellNum &
                        private$plateData[['FAM']] < as.numeric(emptyCutoffs[wellNum]),
                        'cluster'] <- CLUSTER_EMPTY
      NULL
    })
  )
  
  private$plateMeta <-
    private$plateData %>%
    dplyr::filter_(lazyeval::interp(~ cluster == CLUSTER_EMPTY, cluster = quote(cluster))) %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("dropsEmpty" = ~ n()) %>%
    dplyr::left_join(private$plateMeta, ., by = "well") %>%
    dplyr::mutate_("dropsNonEmpty" = ifelse(is.na("dropsEmpty"), NA, "drops - dropsEmpty"),
                   "dropsEmptyFraction" = ifelse(is.na("dropsEmpty"), NA, "signif(dropsEmpty / drops, 3)"))
  
  private$calculateConcentration()
  
  private$status <- STATUS_EMPTY_REMOVED
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to find empty droplets (s): %s",
                    round(tend-tstart)[1]))
  }
  
  invisible(self)
}

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
getEmptyCutoff <- function(wellNum) {
  wellDataSingle <- private$getSingleWell(wellNum, full = TRUE)
  
  # fit two normal distributions in the data along the FAM dimension
  set.seed(SEED)
  quiet(
    mixmdl <- mixtools::normalmixEM(wellDataSingle[['FAM']], k = 2))
  
  # set the FAM cutoff as the mean (mu) of the 1st component + k standard deviations
  smallerComp <- which.min(mixmdl$mu)
  cutoff <- mixmdl$mu[smallerComp] + PARAMS[['EMPTY']][['CUTOFF_SD']] * mixmdl[['sigma']][smallerComp]
  
  wellDataSingle %<>%
    dplyr::filter_(lazyeval::interp(~ FAM < cutoff, FAM = quote(FAM)))

  cutoff %>% ceiling %>% as.integer
}

Plate$set("public", "markEmpty", markEmpty)
Plate$set("private", "getEmptyCutoff", getEmptyCutoff)