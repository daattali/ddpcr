## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains S3 generics that should be implemented when defining
# a new plate type.

#' Parent plate type
#' 
#' Each ddPCR plate has a "parent" plate type from which it inherits all its 
#' properties. When creating a custom plate type, if your plate type inherits from
#' any plate type other than the base type of \code{ddpcr_plate}, you must define
#' this function to return the parent plate type. Inheriting
#' from a parent plate means that the same cluster types, analysis steps, and
#' parameters will be used by default. 
#' 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for more
#' information on plate types.
#' @param plate A ddPCR plate
#' @return The parent type of the given plate.
#' @seealso \code{\link[ddpcr]{type}}\cr
#' \code{\link[ddpcr]{define_params}}\cr
#' \code{\link[ddpcr]{define_clusters}}\cr
#' \code{\link[ddpcr]{define_steps}}
#' @export
#' @keywords internal
parent_plate_type <- function(plate) {
  UseMethod("parent_plate_type")
}

#' Define plate type parameters
#' 
#' Every ddPCR plate type has a set of default parameters. When creating a
#' custom plate type, if your plate type needs a different set of parameters
#' than its parent type, you must define this function to return the parameters
#' specific to this plate. When defining this function, you can use
#' \code{NextMethod("define_params")} to get a list of the parameters of the
#' parent type so that you can simply add to that list rather than redefining
#' all the parameters.
#' @seealso 
#' \code{\link[ddpcr]{params}}
#' @param plate A ddPCR plate
#' @return A list of default parameters for the plate type.
#' @seealso \code{\link[ddpcr]{params}}\cr
#' \code{\link[ddpcr]{parent_plate_type}}\cr
#' \code{\link[ddpcr]{define_clusters}}\cr
#' \code{\link[ddpcr]{define_steps}}
#' @export
#' @keywords internal
define_params <- function(plate) {
  UseMethod("define_params")
}

#' Define droplet clusters
#' 
#' Every ddPCR plate type has a set of potential clusters the droplets can be
#' assigned to. When creating a custom plate type, if your plate type uses a
#' different set of clusters than its parent type, you must define this function
#' to return the cluster names. When defining this function, you can use 
#' \code{NextMethod("define_clusters")} to get a list of the clusters available
#' in the parent type if you want to simply add new clusters without defining
#' all of them.
#' @seealso 
#' \code{\link[ddpcr]{clusters}}
#' @param plate A ddPCR plate
#' @return A list of potential droplet clusters for the plate type.
#' @seealso \code{\link[ddpcr]{clusters}}\cr
#' \code{\link[ddpcr]{parent_plate_type}}\cr
#' \code{\link[ddpcr]{define_params}}\cr
#' \code{\link[ddpcr]{define_steps}}
#' @export
#' @keywords internal
define_clusters <- function(plate) {
  UseMethod("define_clusters")
}

#' Define analysis steps
#' 
#' Every ddPCR plate type has an ordered set of steps that are run to analyze
#' the data. When creating a new plate type, if your plate type has different
#' analysis steps than its parent type, you must define this function to return
#' a named list of the analysis steps. When defining this function, you can use 
#' \code{NextMethod("define_steps")} to get a list of the steps available in
#' the parent type if you want to simply add new steps without defining all of them.
#' @param plate A ddPCR plate
#' @return A named list of analysis steps in the order they should be run on a dataset.
#' The name of each item in the list is the human-readable name of the step
#' and the value of each item is the function to call to perform the step. 
#' @seealso \code{\link[ddpcr]{steps}}\cr
#' \code{\link[ddpcr]{step_begin}}\cr
#' \code{\link[ddpcr]{step_end}}\cr
#' \code{\link[ddpcr]{parent_plate_type}}\cr
#' \code{\link[ddpcr]{define_clusters}}\cr
#' \code{\link[ddpcr]{define_params}}
#' @export
#' @keywords internal
define_steps <- function(plate) {
  UseMethod("define_steps")
}

#' Inform the user that an analysis step is starting
#' 
#' When an analysis step starts running, it's recommended to call this function
#' so that the user will know what step is taking place. The time when a step
#' begins gets recorded so that the user will see exactly how long it took.
#' @param text The text to show the user when this step begins.
#' @seealso \code{\link[ddpcr]{step_end}}
#' @export
#' @keywords internal
step_begin <- function(text) {
  .globals$set("step_tstart", proc.time())
  msg(text, "... ", appendLF = FALSE)
}

#' Inform the user that an analysis step finished
#' 
#' When an analysis step is done, it's recommended to call this function
#' so that the user will know the step finished. The time when a step
#' finishes gets recorded so that the user will see exactly how long it took.
#' An earlier call to \code{step_begin} must have taken place.
#' @param text The text to show the user when this step begins.
#' @seealso \code{\link[ddpcr]{step_begin}}
#' @export
#' @keywords internal
step_end <- function(time) {
  msg(sprintf("DONE (%s seconds)",
                  round(proc.time() - .globals$get("step_tstart"))[1]))
}