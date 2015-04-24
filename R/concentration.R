calculate_concentration_single <- function(plate, well_id) {
  # Calculate concentration in a well, using the formula that QuantaSoft uses
  #
  # Args:
  #   .wellData: The dataframe containing all the droplets
  #   .well: The id of the well of interest
  #
  # Returns:
  #   The concentration in a well as number of copies per templates per microlitre of sample
  total_drops <-
    plate_meta(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    .[['drops']]
  empty_fraction <- 
    plate_meta(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    .[['drops_empty_fraction']]
  total_volume <- params(plate, 'GENERAL', 'DROPLET_VOLUME') * total_drops
  cpd <- -log(empty_fraction) # copies per droplet
  total_templates <- cpd * total_drops
  concentration <- total_templates / total_volume
  concentration %>% as.integer
}

calculate_concentration <- function(plate) {
  concentrations <-
    vapply(wells_success(plate),
           function(x) calculate_concentration_single(plate, x),
           1L) %>%
    as.data.frame %>%
    magrittr::set_colnames("concentration") %>%
    dplyr::mutate_("well" = ~ row.names(.))
  
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(concentrations, "concentration")
  
  plate
}

