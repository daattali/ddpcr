## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' WT-NEG-BRAF assay
#' 
#' "Wild-type negative BRAF" assay type. Use this when initializing a ddPCR
#' plate that uses the WT-NEG-BRAF assay.
#' 
#' TODO what this assay looks like
#' 
#' @examples 
#' \dontrun{
#' new_plate(dir = "dir", type = WTNEGBRAF)
#' }
#' @export
WTNEGBRAF <- "wtnegbraf"

#' @export
#' @keywords internal
parent_plate_type.wtnegbraf <- function(plate) {
  "mutant_wildtype_assay"
}

#' @export
#' @keywords internal
define_params.wtnegbraf <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'X_VAR'              = 'HEX',
      'Y_VAR'              = 'FAM',
      'POSITIVE_DIMENSION' = 'Y'
    )
  )
  params %<>% modifyList(new_params)
    
  params
}

