## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: ddPCR plate
#' 
#' The default plate type that all other plates inherit from. If you initialize
#' a ddPCR plate without specifying a plate type, \code{ddpcr_plate} will be the
#' plate's type.
#' 
#' Plates with this type have the following analysis steps: \code{INITIALIZE},
#' \code{REMOVE_FAILURES}, \code{REMOVE_OUTLIERS}, \code{REMOVE_EMPTY}.
#' 
#' Plates with this type have the following droplet clusters: \code{UNDEFINED},
#' \code{FAILED}, \code{OUTLIER}, \code{EMPTY}.
#' 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}\cr
#' \code{\link[ddpcr]{remove_failures}}\cr
#' \code{\link[ddpcr]{remove_outliers}}\cr
#' \code{\link[ddpcr]{remove_empty}}
#' @name ddpcr_plate
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$ddpcr_plate)
#' type(plate)
#' plate %>% analyze %>% plot
#' } 
NULL

plate_types[['ddpcr_plate']] <- "ddpcr_plate"

#' Parent plate type of default plates
#' @inheritParams parent_plate_type
#' @keywords internal
parent_plate_type.ddpcr_plate <- function(plate) {
  # this is the default plate type -- there is no parent
  NULL
}

#' Parent plate type of any plate
#' @inheritParams parent_plate_type
#' @keywords internal
parent_plate_type.default <- function(plate) {
  # if a plate doesn't have an explicit type, its parent is the default type
  "ddpcr_plate"
}

#' Define plate type parameters for default plates
#' @inheritParams define_params
#' @keywords internal
define_params.ddpcr_plate <- function(plate) {
  # Each parameter has a somewhat descriptive name of what it is used for, and
  # all parameters used by a single step in the pipeline are in a list together
  PARAMS_GENERAL <- list()
  PARAMS_GENERAL['X_VAR'] <- "HEX"
  PARAMS_GENERAL['Y_VAR'] <- "FAM"
  PARAMS_GENERAL['DROPLET_VOLUME'] <- 0.91e-3
  PARAMS_GENERAL['RANDOM_SEED'] <- 8
  PARAMS_REMOVE_OUTLIERS <- list()
  PARAMS_REMOVE_OUTLIERS['TOP_PERCENT'] <- 1
  PARAMS_REMOVE_OUTLIERS['CUTOFF_IQR'] <- 5
  PARAMS_REMOVE_FAILURES <- list()
  PARAMS_REMOVE_FAILURES['TOTAL_DROPS_T'] <- 5000
  PARAMS_REMOVE_FAILURES['EMPTY_LAMBDA_LOW_T'] <- 0.3
  PARAMS_REMOVE_FAILURES['EMPTY_LAMBDA_HIGH_T'] <- 0.99
  PARAMS_REMOVE_EMPTY <- list()
  PARAMS_REMOVE_EMPTY['CUTOFF_SD'] <- 7
  DEFAULT_PARAMS <- list(
    'GENERAL'           = PARAMS_GENERAL,
    'REMOVE_FAILURES'   = PARAMS_REMOVE_FAILURES,
    'REMOVE_OUTLIERS'   = PARAMS_REMOVE_OUTLIERS,
    'REMOVE_EMPTY'      = PARAMS_REMOVE_EMPTY
  )
  DEFAULT_PARAMS
}

#' Define droplet clusters for default plates
#' @inheritParams define_clusters
#' @keywords internal
define_clusters.ddpcr_plate <- function(plate) {
  c(
    'UNDEFINED',
    'FAILED',
    'OUTLIER',
    'EMPTY'
  )
}

#' Define analysis steps for default plates
#' @inheritParams define_steps
#' @keywords internal
define_steps.ddpcr_plate <- function(plate) {
  list(
    'INITIALIZE' = 'init_plate',
    'REMOVE_FAILURES' = 'remove_failures',
    'REMOVE_OUTLIERS' = 'remove_outliers',
    'REMOVE_EMPTY' = 'remove_empty'
  )
}