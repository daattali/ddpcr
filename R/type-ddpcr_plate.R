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
#' \code{\link[ddpcr]{plate_types}}
#' @name ddpcr_plate
#' @usage plate_types$ddpcr_plate
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$ddpcr_plate)
#' type(plate)
#' } 
NULL

plate_types[['ddpcr_plate']] <- "ddpcr_plate"