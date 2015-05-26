## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' Get borders of filled droplets in PPNP assay
#' 
#' In a PPNP assay, the rain droplets are the non-empty drops that don't have a
#' high enough intensity in the positive dimension to be considered as filled
#' with high quality sample DNA. Only droplets considered as filled are
#' candidates for the \code{negative} and \code{positive} clusters.
#' \code{get_filled_borders} returns the threshold values in the positive
#' dimension that are used to determine which drops are filled.
#' 
#' @param plate A ddPCR plate.
#' @param well_id Get borders of filled droplets for this well.
#' @return A 2-element numeric vector defining the thresholds of filled drops
#' in the positive dimension.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' get_filled_borders(plate, "B06")
#' get_filled_borders(plate, "C09")
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}},
#' @seealso \code{\link[ddpcr]{positive_dim}},
#' @seealso \code{\link[ddpcr]{get_filled_drops}}
#' @keywords internal
#' @export
get_filled_borders <- function(plate, well_id) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  
  well_data <- get_single_well(plate, well_id)
  
  set.seed(SEED)
  
  # Fit two normal distributions in the positive dimension of all non-empty
  # drops. The higher population will model the filled droplets, and we define
  # the borders as the center of the higher population plus/minus X standard
  # deviations (3 by default)
  positive_var <- positive_dim_var(plate)
  quiet(
    mixmdl_pos <- mixtools::normalmixEM(well_data[[positive_var]], k = 2))
  larger_comp_pos <- mixmdl_pos$mu %>% which.max
  filled_borders <-
    plus_minus(
      mixmdl_pos$mu[larger_comp_pos],
      mixmdl_pos$sigma[larger_comp_pos] *
        params(plate, 'CLASSIFY', 'CLUSTERS_BORDERS_NUM_SD')
    ) %>%
    as.integer
  
  filled_borders
}

#' Get filled droplets in PPNP assay
#' 
#' In a PPNP assay, the rain droplets are the non-empty drops that don't have a
#' high enough intensity in the positive dimension to be considered as filled
#' with high quality sample DNA. Only droplets considered as filled are
#' candidates for the \code{negative} and \code{positive} clusters.
#' \code{get_filled_drops} returns the droplets that are considered filled.
#' 
#' @param plate A ddPCR plate.
#' @param well_id Get borders of filled droplets for this well.
#' @param borders (Optional) The filled droplets borders, as calculated by
#' \code{\link[ddpcr]{get_filled_drops}}. If missing, then
#' \code{\link[ddpcr]{get_filled_drops}} is called to calculate the borders.
#' @return Dataframe with all filled droplets in the given well.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' get_filled_drops(plate, "B06")
#' get_filled_drops(plate, "B06", get_filled_borders(plate, "B06"))
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}},
#' @seealso \code{\link[ddpcr]{positive_dim}},
#' @seealso \code{\link[ddpcr]{get_filled_drops}}
#' @keywords internal
#' @export
get_filled_drops <- function(plate, well_id, borders) {
  if (missing(borders)) {
    borders <- get_filled_borders(plate, well_id)
  }
  
  well_data <- get_single_well(plate, well_id)
  well_data %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% borders,
                                    var = as.name(positive_dim_var(plate)))
    )
}
