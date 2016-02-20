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
#' Plates with this type have the following analysis steps: \code{INITIALIZE},
#' \code{REMOVE_FAILURES}, \code{REMOVE_OUTLIERS}, \code{REMOVE_EMPTY},
#' \code{CLASSIFY}, \code{RECLASSIFY}.
#' 
#' Plates with this type have the following droplet clusters:
#' \code{UNDEFINED}, \code{FAILED}, \code{OUTLIER}, \code{EMPTY} (double-negative),
#' \code{RAIN} (filled but not wildtype nor negative), \code{POSITIVE} (wildtype),
#' \code{NEGATIVE} (mutant).
#' 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}\cr
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}\cr
#' \code{\link[ddpcr]{hex_positive_pnpp}}\cr
#' \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{remove_failures}}\cr
#' \code{\link[ddpcr]{remove_outliers}}\cr
#' \code{\link[ddpcr]{remove_empty}}\cr
#' \code{\link[ddpcr]{classify_droplets}}\cr
#' \code{\link[ddpcr]{reclassify_droplets}}
#' @name fam_positive_pnpp
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp)
#' type(plate)
#' plate %>% analyze %>% plot
#' } 
NULL

plate_types[['fam_positive_pnpp']] <- "fam_positive_pnpp"

#' Parent plate type of FAM-positive PNPP 
#' @inheritParams parent_plate_type
#' @keywords internal
parent_plate_type.fam_positive_pnpp <- function(plate) {
  "wildtype_mutant_pnpp"
}

#' Define plate type parameters for FAM-positive PNPP 
#' @inheritParams define_params
#' @keywords internal
define_params.fam_positive_pnpp <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_DIMENSION' = 'Y'
    )
  )
  params %<>% utils::modifyList(new_params)
    
  params
}