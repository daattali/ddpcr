## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: HEX-positive PNPP 
#' 
#' A ddPCR plate of type \code{hex_positive_pnpp}, which can also be expressed as
#' (HEX+)/(FAM+HEX+), is a subtype of both \code{\link[ddpcr]{pnpp_experiment}}
#' and \code{\link[ddpcr]{wildtype_mutant_pnpp}}. Use this plate type if your data
#' has three main clusters of droplets: double-negative (empty droplets),
#' FAM+HEX+ (wildtype droplets) and HEX+FAM- (mutant droplets). 
#' 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}
#' \code{\link[ddpcr]{fam_positive_pnpp}}
#' @name hex_positive_pnpp
#' @usage plate_types$hex_positive_pnpp
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$hex_positive_pnpp)
#' type(plate)
#' plate %>% analyze %>% plot
#' } 
NULL

plate_types[['hex_positive_pnpp']] <- "hex_positive_pnpp"

#' Parent plate type of HEX-positive PNPP 
#' @inheritParams parent_plate_type
parent_plate_type.hex_positive_pnpp <- function(plate) {
  "wildtype_mutant_pnpp"
}

#' Define plate type parameters for HEX-positive PNPP 
#' @inheritParams define_params
define_params.hex_positive_pnpp <- function(plate) {
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
