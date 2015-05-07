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
#' dir <- system.file("sample_data", "small", package = "ddpcrS3")
#' new_plate(dir = dir, type = KRAS)
#' new_plate(dir = dir, type = "kras")
#' @seealso \code{\link[ddpcrS3]{new_plate}}
#' @export
KRAS <- "kras"

#' Define parent plate type of KRAS assay
#' @seealso
#' \code{\link[ddpcrS3]{parent_plate_type}},
#' \code{\link[ddpcrS3]{KRAS}}
#' @export
#' @keywords internal
parent_plate_type.kras <- function(plate) {
  "mutant_wildtype_assay"
}

#' Define default parameters of KRAS assay
#' @seealso
#' \code{\link[ddpcrS3]{define_params}},
#' \code{\link[ddpcrS3]{KRAS}}
#' @export
#' @keywords internal
define_params.kras <- function(plate) {
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