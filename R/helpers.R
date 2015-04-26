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
get_single_well <- function(plate, well_id,
                            empty = FALSE, outliers = FALSE, clusters = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  well_id %<>% toupper
  
  result <-
    plate_data(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    dplyr::select_(quote(-well))
  
  if (!empty) {
    result %<>%
      dplyr::filter_(~ cluster != CLUSTER_EMPTY)
  }
  if (!outliers) {
    result %<>%
      dplyr::filter_(~ cluster != CLUSTER_OUTLIER)
  }
  if (!clusters) {
    result %<>%
      dplyr::select_(~ -cluster)
  }
  
  result
}





