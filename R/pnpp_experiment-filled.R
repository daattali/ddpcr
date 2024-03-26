## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Get border of filled droplets in PNPP experiment
#' 
#' In a PNPP experiment, the rain droplets are the non-empty drops that don't have a
#' high enough intensity in the positive dimension to be considered as filled
#' with high quality sample DNA. Only droplets considered as filled are
#' candidates for the \code{negative} and \code{positive} clusters.
#' \code{get_filled_border} returns the threshold value in the positive
#' dimension that is used to determine which drops are filled.
#' 
#' @param plate A ddPCR plate.
#' @param well_id Get border of filled droplets for this well.
#' @return Thresholds of filled drops in the positive dimension.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' get_filled_border(plate, "A05")
#' get_filled_border(plate, "F05")
#' @seealso \code{\link[ddpcr]{pnpp_experiment}}\cr
#' \code{\link[ddpcr]{positive_dim}}\cr
#'\code{\link[ddpcr]{get_filled_drops}}
#' @keywords internal
#' @export
get_filled_border <- function(plate, well_id) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  
  well_data <- get_single_well(plate, well_id)
  
  set.seed(params(plate, 'GENERAL', 'RANDOM_SEED'))
  
  # Fit two normal distributions in the positive dimension of all non-empty
  # drops. The higher population will model the filled droplets, and we define
  # the border as the center of the higher population minus X standard
  # deviations (3 by default)
  positive_var <- positive_dim_var(plate)
  quiet(
    mixmdl_pos <- mixtools::normalmixEM(well_data[[positive_var]], k = 2))
  larger_comp_pos <- mixmdl_pos$mu %>% which.max
  filled_border <-
    mixmdl_pos$mu[larger_comp_pos] - 
    (mixmdl_pos$sigma[larger_comp_pos] *
    params(plate, 'CLASSIFY', 'CLUSTERS_BORDERS_NUM_SD'))
  filled_border <- as.integer(filled_border)
  
  filled_border
}

#' Get filled droplets in PNPP experiment
#' 
#' In a PNPP experiment, the rain droplets are the non-empty drops that don't have a
#' high enough intensity in the positive dimension to be considered as filled
#' with high quality sample DNA. Only droplets considered as filled are
#' candidates for the \code{negative} and \code{positive} clusters.
#' \code{get_filled_drops} returns the droplets that are considered filled.
#' 
#' @param plate A ddPCR plate.
#' @param well_id Get border of filled droplets for this well.
#' @param border (Optional) The filled droplets border, as calculated by
#' \code{\link[ddpcr]{get_filled_drops}}. If missing, then
#' \code{\link[ddpcr]{get_filled_drops}} is called to calculate the border.
#' @return Dataframe with all filled droplets in the given well.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' get_filled_drops(plate, "A05")
#' get_filled_drops(plate, "A05", get_filled_border(plate, "A05"))
#' @seealso \code{\link[ddpcr]{pnpp_experiment}}\cr
#' \code{\link[ddpcr]{positive_dim}}\cr
#' \code{\link[ddpcr]{get_filled_drops}}
#' @keywords internal
#' @export
get_filled_drops <- function(plate, well_id, border) {
  if (missing(border)) {
    border <- get_filled_border(plate, well_id)
  }
  
  well_data <- get_single_well(plate, well_id)
  well_data %>%
    dplyr::filter(.data[[positive_dim_var(plate)]] >= border)
}
