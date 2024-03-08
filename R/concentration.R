## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Calculate template concentration
#' 
#' Calculate template concentration in each wells in a plate using the same formula that
#' QuantaSoft uses. The concentration information is added to the plate's metadata.
#' 
#' The concentration in a well as number of copies of template per microlitre
#' of sample, and uses the following equation:
#' 
#' (-log(drops_empty / drops_total) * drops_total) / (droplet_volume * drops_total)
#' @return A ddPCR plate with the metadata containing a new `concentration` variable.
#' @keywords internal
#' @export
calculate_concentration <- function(plate) {
  concentrations <-
    vapply(
      wells_success(plate),
      function(x) calculate_concentration_single(plate, x),
      integer(1)      
    ) %>%
    named_vec_to_df("concentration")
  
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(concentrations, "concentration")
  
  plate
}

#' Calculate concentration in a single well
#' @return The concentration (integer) in a well.
#' @keywords internal
#' @export
calculate_concentration_single <- function(plate, well_id) {
  total_drops <-
    plate_meta(plate) %>%
    dplyr::filter(well == well_id) %>%
    .[['drops']]
  
  empty_fraction <- 
    plate_meta(plate) %>%
    dplyr::filter(well == well_id) %>%
    .[['drops_empty_fraction']]
  
  total_volume <- params(plate, 'GENERAL', 'DROPLET_VOLUME') * total_drops
  cpd <- -log(empty_fraction) # copies per droplet
  total_templates <- cpd * total_drops
  concentration <- total_templates / total_volume
  concentration %>% as.integer
}
