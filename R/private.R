# Retrieve droplet data for a single well.
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .full: If true, get all droplets, including the empty ones
#   .clusters: If true, return information about which cluster each drop belongs to
#
# Returns:
#   Dataframe containing only droplet data for the given well
getSingleWell <- function(wellNum, full = FALSE, clusters = FALSE) {
  wellDataSingle <-
    self$getData() %>%
    dplyr::filter_(lazyeval::interp(~ well == wellNum, well = quote(well))) %>%
    dplyr::select_(quote(-well))
  
  if (!full) {
    wellDataSingle %<>%
      dplyr::filter_(lazyeval::interp(~ cluster != CLUSTER_EMPTY, cluster = quote(cluster)))
  }
  
  if (!clusters) {
    wellDataSingle %<>%
      dplyr::select_(quote(-cluster))
  }
  
  wellDataSingle
}

calculateMtFreqs <- function() {
  mtFreqs <-
    vapply(private$wellsSuccess(),
           private$calcMTFreqSingle,
           1) %>%
    signif(3) %>%
    as.data.frame %>%
    magrittr::set_colnames("mtFreq") %>%
    dplyr::mutate_("well" = ~ row.names(.))
  
  # if we already have a mtFreq column, remove it first to not end up with two
  if ("mtFreq" %in% colnames(self$getMeta())) {
    private$plateMeta %<>% dplyr::select_(quote(-mtFreq))
  }
  
  private$plateMeta %<>%
    dplyr::left_join(mtFreqs, by = "well")
}

calcMTFreqSingle <- function(wellNum) {
  wellDataSingle <- private$getSingleWell(wellNum, clusters = TRUE)
  numMT <- sum(wellDataSingle[['cluster']] == CLUSTER_MT)
  numWT <- sum(wellDataSingle[['cluster']] == CLUSTER_WT)
  mtFreq <- numMT / (numMT + numWT) * 100
  mtFreq
}

"%btwn%" <- function(x, rng) {
  stopifnot(is.numeric(x), is.numeric(rng), length(rng) == 2)
  rng %<>% sort
  x >= rng[1] & x <= rng[2]
}

Plate$set("private", "getSingleWell", getSingleWell)
Plate$set("private", "calculateMtFreqs", calculateMtFreqs)
Plate$set("private", "calcMTFreqSingle", calcMTFreqSingle)