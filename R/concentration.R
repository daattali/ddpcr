calculateConcentration <- function() {
  concentrations <-
    vapply(private$wellsSuccess(),
           private$calculateConcentrationSingle,
           1) %>%
    as.data.frame %>%
    magrittr::set_colnames("concentration") %>%
    dplyr::mutate_("well" = ~ row.names(.))
  
  private$plateMeta %<>%
    dplyr::left_join(concentrations, by = "well")
}

calculateConcentrationSingle <- function(wellNum) {
  # Calculate concentration in a well, using the formula that QuantaSoft uses
  #
  # Args:
  #   .wellData: The dataframe containing all the droplets
  #   .well: The id of the well of interest
  #
  # Returns:
  #   The concentration in a well as number of copies per templates per microlitre of sample
  totalDrops <- private$plateMeta %>%
    dplyr::filter_(lazyeval::interp(~ well == wellNum, well = quote(well))) %>%
    .[['drops']]
  emptyFraction <- private$plateMeta %>%
    dplyr::filter_(lazyeval::interp(~ well == wellNum, well = quote(well))) %>%
    .[['dropsEmptyFraction']]
  
  totalVolume <- PARAMS[['PCR']][['DROPLET_VOLUME']] * totalDrops
  cpd <- -log(emptyFraction) # copies per droplet
  totalTemplates <- cpd * totalDrops
  concentration <- totalTemplates / totalVolume
  concentration
}

Plate$set("private", "calculateConcentration", calculateConcentration)
Plate$set("private", "calculateConcentrationSingle", calculateConcentrationSingle)