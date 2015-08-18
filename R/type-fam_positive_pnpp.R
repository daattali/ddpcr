## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: FAM-positive PNPP 
#' 
#' A ddPCR plate of type \code{fam_positive_pnpp}, which can also be expressed as
#' (FAM+)/(FAM+HEX+), is a subtype of both \code{\link[ddpcr]{pnpp_experiment}}
#' and \code{\link[ddpcr]{wildtype_mutant_pnpp}}. Use this plate type if your data
#' has three main clusters of droplets: double-negative (empty droplets),
#' FAM+HEX+ (wildtype droplets) and FAM+HEX- (mutant droplets). 
#' 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}
#' \code{\link[ddpcr]{hex_positive_pnpp}}
#' @name fam_positive_pnpp
#' @usage plate_types$fam_positive_pnpp
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$fam_positive_pnpp)
#' type(plate)
#' plate %>% analyze %>% plot
#' } 
NULL

plate_types[['fam_positive_pnpp']] <- "fam_positive_pnpp"

#' Parent plate type of FAM-positive PNPP 
#' @inheritParams parent_plate_type
parent_plate_type.fam_positive_pnpp <- function(plate) {
  "wildtype_mutant_pnpp"
}

#' Define plate type parameters for FAM-positive PNPP 
#' @inheritParams define_params
define_params.fam_positive_pnpp <- function(plate) {
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