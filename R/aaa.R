## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# metadata of an empty plate
DEFAULT_PLATE_META <-
  expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
  magrittr::set_colnames(c("row", "col")) %>%
  dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                 "sample" = NA, "used" = FALSE) %>%
  dplyr::select_("well", "sample", "row", "col", "used")

#' Supported plate types
#' 
#' Each ddPCR plate has a plate type which determines what type of analysis to run
#' on the data. \code{plate_types} is a list containing the plate types that are
#' supported. If no plate type is specified, the default assumed type is
#' \code{ddpcr_plate}.\cr\cr
#' The most useful built-in plate types are:
#' \code{\link[ddpcr]{fam_positive_pnpp}},
#' \code{\link[ddpcr]{hex_positive_pnpp}},
#' \code{\link[ddpcr]{custom_thresholds}}.\cr\cr
#' For full details on the differences between plate types or to learn how to
#' add a new plate type, \href{https://github.com/daattali/ddpcr}{see the package README}.
#' @seealso \code{\link[ddpcr]{new_plate}}\cr
#' \code{\link[ddpcr]{fam_positive_pnpp}}\cr
#' \code{\link[ddpcr]{hex_positive_pnpp}}\cr
#' \code{\link[ddpcr]{custom_thresholds}}\cr
#' \code{\link[ddpcr]{pnpp_experiment}}\cr
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}\cr
#' \code{\link[ddpcr]{ddpcr_plate}}\cr
#' \code{\link[ddpcr]{type}}
#' @format NULL 
#' @usage NULL
#' @examples 
#' \dontrun{
#' dir <- sample_data_dir()
#' new_plate(dir, type = plate_types$ddpcr_plate)
#' new_plate(dir, type = plate_types$custom_thresholds)
#' new_plate(dir, type = plate_types$fam_positive_pnpp)
#' }
#' @export
plate_types <- list()