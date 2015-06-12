## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' KRAS assay
#' 
#' "KRAS" assay type. Use this when initializing a ddPCR
#' plate that uses the KRAS assay.
#' 
#' TODO what this assay looks like
#' 
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' new_plate(dir = dir, type = HEX_POSITIVE_PPNP)
#' @seealso
#' \code{\link[ddpcr]{new_plate}},
#' \code{\link[ddpcr]{PPNP_ASSAy}}
#' @export
HEX_POSITIVE_PPNP <- "hex_positive_ppnp"

#' @export
parent_plate_type.hex_positive_ppnp <- function(plate) {
  "wildtype_mutant_ppnp"
}

#' @export
define_params.hex_positive_ppnp <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'X_VAR' = "HEX",
      'Y_VAR' = "FAM",
      'POSITIVE_DIMENSION' = 'X'
    )
  )
  params %<>% modifyList(new_params)
  
  params
}
