## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains core ddPCR plate functions. All these functions are exported
# and should be used by users.

#' Create a new ddPCR plate
#' 
#' Any ddPCR analysis must start by creating a ddPCR plate object. Use this
#' function to read ddPCR data into R and create a plate object that can then
#' be analyzed.
#' 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for more
#' information on plate types.
#' 
#' @param dir The directory containing the ddPCR droplet data files, and potentially
#' the plate results file
#' @param type A ddPCR plate type (see \code{\link[ddpcr]{plate_types}})
#' @param data_files If \code{dir} is not provided, you can provide a vector of
#' file paths to the ddPCR droplet data files.
#' @param meta_file If \code{dir} is not provided, you can provide a file path
#' to the ddPCR results file.
#' @param name Name of the dataset. If not provided, the name will be guessed
#' based on the filenames.
#' @param params List of parameters to set for the plate. Only advanced users
#' should consider using this feature.
#' @return A new ddPCR plate object with droplet data loaded that is ready
#' to be analyzed.
#' 
#' @section Providing ddPCR data:
#' The first step to using the \code{ddpcr} package is to get the ddPCR data into
#' R. This package uses as input the data files that are exported by QuantaSoft.
#' For a dataset with 20 wells, QuantaSoft will create 20 well files (each ending
#' with "_Amplitude.csv") and one results file. The well files are essential for
#' analysis since they contain the actual droplet data, and the results file
#' is optional because the only information used from it is the mapping from
#' well IDs to sample names.
#' 
#' The easiest way to use your ddPCR data with this package is to Export the data
#' from QuantaSoft into some directory, and providing that directory as the 
#' \code{dir} argument.  This way, this package will automatically find all the
#' data files as well as the results file.  Alternatively, you can provide the
#' data files (well files) manually as a list of filenames using the \code{data_files}
#' argument. If you use the \code{data_files} argument instead of \code{dir}, you
#' can also optionally provide the results file as the \code{meta_file} argument.
#' If no results file is provided then the wells will not be mapped to their sample
#' names.
#' 
#' @section Plate parameters:
#' Every plate has a set of default parameters that are used in the analysis.
#' You can see all the parameters of a plate with the \code{\link[ddpcr]{params}}
#' function. If you want to provide different values for some parameters when
#' initializing a plate, you can do that with the \code{params} argument. This
#' is considered an advanced feature.
#'
#' For example, if you inspect the parameters of any ddPCR plate, you will see that
#' by defalt the random seed used by default is 8. If you want to create
#' a new plate that uses a different random seed, you could do so like this:
#' \preformatted{
#' plate <- new_plate(sample_data_dir(), params = list('GENERAL' = list('RANDOM_SEED' = 10)))
#' plate %>% p
#' } 
#' 
#' Most numeric parameters that are used in the algorithms of the analysis steps
#' can be modified in a similar fashion. This can be used to fine-tune the 
#' analysis of a plate if you require different parameters.
#' 
#' @seealso \code{\link[ddpcr]{plate_types}}\cr
#' \code{\link[ddpcr]{type}}\cr
#' \code{\link[ddpcr]{reset}}\cr
#' \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{plot.ddpcr_plate}}\cr
#' \code{\link[ddpcr]{params}}
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir())
#' } 
#' @export
new_plate <- function(dir, type, data_files, meta_file, name, params) {
  plate <- setup_new_plate(type)
  plate <- read_plate(plate, dir, data_files, meta_file)
  
  # If a name was given, use it instead of the automatically inferred name
  if (!missing(name)) {
    name(plate) <- name
  }
  
  if (!missing(params)) {
    params(plate) %<>% utils::modifyList(params)
  }
  
  plate <- init_plate(plate)
  
  plate
}

#' Reset a plate
#' 
#' Reset a ddPCR plate object back to its original state. After resetting a plate,
#' all the analysis progress will be lost, but the original droplet data and
#' plate metadata will be kept. Two common reasons to reset a plate are either
#' to restart the analysis, or to re-analyze the plate as a different plate type.
#' 
#' @param plate A ddPCR plate
#' @param type A ddPCR plate type (see \code{\link[ddpcr]{plate_types}})
#' @param params List of parameters to set for the plate. Only advanced users
#' should consider using this feature. See \code{\link[ddpcr]{new_plate}} for usage.
#' @param keep_type If \code{TRUE} then use keep the same plate type as \code{plate}
#' @param keep_params If \code{TRUE} then keep the same plate parameters of \code{plate}
#' @return A new unanalyzed ddPCR plate 
#' @seealso \code{\link[ddpcr]{plate_types}}\cr
#' \code{\link[ddpcr]{new_plate}}
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' plate <- reset(plate, type=plate_types$fam_positive_pnpp)
#' } 
#' @export
reset <- function(plate, type, params,
                  keep_type = FALSE, keep_params = FALSE) {
  if (keep_type) {
    type <- NULL
    type <- type(plate)
  }
  if (keep_params) {
    params <- NULL
    params <- params(plate)
  }
  class(plate) <- NULL
  plate <- setup_plate(plate, type)
  plate <- init_plate(plate)
  if (!missing(params)) {
    params(plate) %<>% utils::modifyList(params)
  }
  plate
}

#' Reset plate parameters to their defaults
#' 
#' Use this function to reset a ddPCR plate's parameters back to their default
#' values.
#' @param plate A ddPCR plate.
#' @return The plate with the parameters set to the plate type's default values.
#' @seealso \code{\link[ddpcr]{params}}
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' x_var(plate) <- "VIC"
#' plate <- set_default_params(plate)
#' } 
#' @export
set_default_params <- function(plate) {
  if (!is_empty_plate(plate)) {
    new_params <- define_params(plate)
    x_var(plate) <- new_params[['GENERAL']]['X_VAR']
    y_var(plate) <- new_params[['GENERAL']]['Y_VAR']
  }

    params(plate) <- define_params(plate)
  plate
}

#' Get metadata info of a well
#' 
#' Each ddPCR plate has associated metadata that stores infromation for every well.
#' Use this function to retrieve any metadata information for a single well or
#' for a list of wells.
#' 
#' @param plate A ddPCR plate
#' @param well_ids A character vecotr of well IDs denoting the wells to get information
#' for
#' @param var The metadata variable to get (to see a list of all possible metadata
#' variables, use \code{names(plate_meta(plate))})
#' @return A character vector with the wanted metadata variable value for each
#' well.
#' @seealso \code{\link[ddpcr]{plate_meta}}
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' well_info(plate, "A01", "drops")
#' } 
#' @export
well_info <- function(plate, well_ids, var) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate_meta(plate) %>%
    dplyr::filter(.data[["well"]] %in% well_ids) %>%
    .[[var]]
}

#' Get wells used in a ddPCR plate
#' 
#' Get a list of the wells that have any data in a ddPCR plate.
#' @param plate A ddPCR plate
#' @return List of wells that have any data in the given plate.
#' @seealso \code{\link[ddpcr]{subset.ddpcr_plate}}
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' wells_used(plate)
#' plate <- subset(plate, "A01:C05")
#' wells_used(plate)
#' } 
#' @export
wells_used <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate %>%
    plate_meta %>%
    dplyr::filter(.data[["used"]]) %>%
    .[['well']]
}

#' Run analysis on a ddPCR plate
#'
#' Every ddPCR plate has a set of defined steps that are taken in order, that
#' together constitute "analyzing" the plate.  Calling the \code{analyze} function
#' will perform all the analysis steps, which may take several minutes. Running
#' the analysis will classify the droplets in the plate into clusters (available
#' via \code{\link[ddpcr]{plate_data}}) and will add variables to the plate
#' metadata (available via \code{\link[ddpcr]{plate_meta}}).
#' 
#' This function will run an analysis to completion. If you want to run each
#' step one at a time, use \code{\link[ddpcr]{next_step}}. 
#' 
#' @param plate A ddPCR plate
#' @param restart If \code{TRUE}, then run the analysis from the beginning;
#' othrewise, continue from the last step that was performed.
#' @return The analyzed ddPCR plate
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' plate <- analyze(plate)
#' } 
#' @seealso
#' \code{\link[ddpcr]{next_step}}\cr
#' \code{\link[ddpcr]{plot.ddpcr_plate}}\cr
#' \code{\link[ddpcr]{new_plate}}\cr
#' \code{\link[ddpcr]{steps}}\cr
#' \code{\link[ddpcr]{plate_data}}\cr
#' \code{\link[ddpcr]{plate_meta}}
#' @note Most analysis steps result in some progress messages being printed to
#' the screen. You can turn off these messages by disabling the verbose option
#' with the command \code{options(ddpcr.verbose = FALSE)}.
#' @export
analyze <- function(plate, restart = FALSE) {
  if (restart) {
    msg("Restarting analysis")
    status(plate) <- 0
    plate[['dirty']] <- FALSE
  }
  steps_left <- length(steps(plate)) - status(plate)
  plate %<>% next_step(n = steps_left)
  msg("Analysis complete")
  plate
}


#' Run the next step in an analysis
#'
#' Every ddPCR plate has a set of defined steps that are taken in order, that
#' together constitute "analyzing" the plate.  Calling the \code{next_step} function
#' will run the next step in the analysis, which may take several minutes. If you
#' want to run all the remaining steps at once, use \code{\link[ddpcr]{analyze}} instead.
#' 
#' @param plate A ddPCR plate
#' @param n The number of steps to run
#' @return The ddPCR plate after running the next step
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
#' plate <- next_step(plate)
#' } 
#' @seealso \code{\link[ddpcr]{plot.ddpcr_plate}}\cr
#' \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{steps}}\cr
#' \code{\link[ddpcr]{plate_data}}\cr
#' \code{\link[ddpcr]{plate_meta}}
#' @export
next_step <- function(plate, n = 1) {
  if (n == 0) {
    return(plate)
  }
  
  if (analysis_complete(plate)) {
    msg("Analysis complete")
    return(plate)
  }
  
  next_step_name <-
    plate %>%
    status %>% 
    magrittr::add(1) %>%
    step_name(plate, .)
  next_step_fxn <-
    plate %>%
    steps %>%
    .[[next_step_name]]
  
  plate <- do.call(next_step_fxn, list(plate))
  
  next_step(plate, n - 1)
}