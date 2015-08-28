## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains functions that get/set ddPCR plate object attributes and
# related functions. Many of these functions are exported and should be used
# by users to get/set plate data.

#' Plate type
#' 
#' Get the type of a ddPCR plate. 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for more
#' information on plate types.
#' @param plate A ddPCR plate
#' @param all If \code{FALSE}, show only the most specific plate type; otherwise,
#' show all inherited (implicit) types as well.
#' @return A character vector with the plate type(s).
#' @seealso \code{\link[ddpcr]{plate_types}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$fam_positive_pnpp)
#' type(plate)
#' type(plate, TRUE)
#' } 
#' @export
type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}

#' Plate data (droplets data)
#' 
#' The main piece of information in every ddPCR plate is the droplets data,
#' which contains the fluorescence intensities for every single droplet in
#' every well. After a ddPCR plate gets analyzed, this data also includes the
#' assigned cluster for each droplet. The plate data may be useful programatically,
#' but it's not very useful to a human, so if you want to visualize the plate data
#' you should instead plot it using \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @param plate A ddPCR plate
#' @return A dataframe containing all the droplets in the plate, along with
#' the assigned cluster of each droplet.
#' @seealso \code{\link[ddpcr]{plate_meta}}\cr
#' \code{\link[ddpcr]{plot.ddpcr_plate}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' plate_data(plate)
#' } 
#' @export
plate_data <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['plate_data']]
}

#' Overwrite the plate data
#' 
#' When creating custom analysis steps for new plate types, it is often necessary
#' to set the plate data with new data, especially when assigning new clusters
#' to the plate droplets.
#' @param plate A ddPCR plate
#' @param value New plate data
#' @seealso \code{\link[ddpcr]{plate_data}}
#' @export
#' @keywords internal
`plate_data<-` <- function(plate, value) {
  plate[['plate_data']] <- value
  plate
}

#' Plate status
#' 
#' The status of a plate corresponds to the number of analysis steps that have
#' taken place. A plate that has been initialized but has not yet been analyzed
#' at all has status 1.\cr\cr
#' If you add custom analysis steps to a new plate type, you should make sure
#' to update the status of the plate after each step. You can use 
#' \code{\link[ddpcr]{check_step}} to ensure that the plate is at an appropriate
#' status before beginning each step.
#' @seealso \code{\link[ddpcr]{steps}}\cr
#' \code{\link[ddpcr]{check_step}}
#' @name status
NULL

#' @rdname status
#' @export
#' @keywords internal
status <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['status']]
}
#' @rdname status
#' @export
#' @keywords internal
`status<-` <- function(plate, value) {
  plate[['status']] <- value
  plate
}

#' Is a plate empty?
#' 
#' A plate is considered empty if it has not yet been fully initialized, and thus
#' its status is 0.
#' @seealso \code{\link[ddpcr]{status}}
#' @export
#' @keywords internal
is_empty_plate <- function(plate) {
  is.null(status(plate))
}

#' Is the analysis complete?
#' 
#' Check if a ddPCR plate has been fully analyzed or if there are remaining steps.
#' 
#' @param plate A ddPCR plate
#' @return \code{TRUE} if the plate's analysis has been fully carried out;
#' \code{FALSE} otherwise. 
#' @seealso \code{\link[ddpcr]{status}}\cr
#' \code{\link[ddpcr]{analyze}}
#' @export
analysis_complete <- function(plate) {
  status(plate) == length(steps(plate))
}

#' Plate metadata 
#'
#' The metadata is a collection of variables that describe each well in the plate.
#' The metadata of an unanalyzed plate only contains basic information about each
#' well, such as the sample name, whether the well was used, and the number of 
#' droplets in the well. Analyzing a plate adds many more variables to the metadata,
#' such as the number of empty droplets, the number of outliers, the template 
#' concentration, and more.
#' 
#' @param plate A ddPCR plate
#' @param only_used If \code{TRUE}, only return metadata for wells that are
#' used in this plate (wells that have any data)
#' @return A dataframe containing the plate metadata
#' @seealso \code{\link[ddpcr]{plate_data}}
#' \code{\link[ddpcr]{plot.ddpcr_plate}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' plate %>% plate_meta(only_used = TRUE)
#' plate %>% analyze %>% plate_meta(only_used = TRUE)
#' } 
#' @export
plate_meta <- function(plate, only_used = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (only_used) {
    plate[['plate_meta']] %>% dplyr::filter_(quote(used))
  } else {  
    plate[['plate_meta']]
  }
}
#' Overwrite the plate metadata
#' 
#' When creating custom analysis steps for new plate types, it is often necessary
#' to add/change variables in the plate metadata.
#' @param plate A ddPCR plate
#' @param value New plate metadata
#' @seealso \code{\link[ddpcr]{plate_meta}}
#' @export
#' @keywords internal
`plate_meta<-` <- function(plate, value) {
  plate[['plate_meta']] <- value
  plate
}
# Arrange the metadata such that the more general variables are near the beginning
arrange_meta <- function(plate) {
  if ("success" %in% colnames(plate)) {
    plate %>% dplyr::arrange_(~ desc(used), ~ desc(success), ~ row, ~ col)  
  } else {
    plate %>% dplyr::arrange_(~ desc(used), ~ row, ~ col)
  }
}

#' Plate name
#' 
#' Get or set the name of a dataset.
#' @param plate A ddpcrPlate
#' @param value New name
#' @return Plate name
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' name(plate)
#' name(plate) <- "foo"
#' name(plate)
#' }
#' @name name
NULL

#' @rdname name
#' @export
name <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['name']]
}
#' @rdname name
#' @export
`name<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['name']] <- value
  plate
}

#' Plate parameters
#' 
#' Every ddPCR plate object has adjustable parameters associated with it.
#' Each parameter belongs to a category of parameters, and has a unique name.
#' For example, there are general parameters (category 'GENERAL') that apply to
#' the plate as a whole, and each analysis step has its own set of parameters
#' that are used for the algorithm in that step.\cr\cr
#' You can either view all parameters of a plate by not providing any arguments,
#' view all parameters in a category by providing the category, or view the value
#' of a specific parameter by providing both the category and the parameter name.\cr\cr
#' 
#' Setting new parameter values should only be done by advanced users.
#' Note that if you change any parameters, you need to re-run the analysis in order
#' for the parameter changes to take effect.
#' 
#' Tip: it can be easier to visually inspect the parameters by wrapping the
#' return value in a \code{str()}.
#' 
#' Warning: Do not directly set the GENERAL-X_VAR or GENERAL-Y_VAR parameters.
#' Instead, use \code{\link[ddpcr]{x_var}} or \code{\link[ddpcr]{y_var}}.
#' @param plate A ddPCR plate
#' @param category Category of parameters
#' @param name Parameter name
#' @param value New parameter value
#' @return If no category is provided, return all parameters. If a category is provided,
#' return all parameters in that category. If both a category and a name are provided,
#' return the value of the specific parameter.
#' @seealso \code{\link[ddpcr]{x_var}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' 
#' # retrieving plate parameters
#' str(params(plate))
#' str(params(plate, 'GENERAL'))
#' params(plate, 'GENERAL', 'RANDOM_SEED')
#' 
#' # setting plate parameters
#' params(plate, 'GENERAL', 'RANDOM_SEED') <- 10
#' str(params(plate, 'GENERAL'))
#' }
#' @name params
NULL

#' @rdname params
#' @export
params <- function(plate, category, name) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  res <- plate[['params']]
  if (!missing(category)) {
    res <- res[[category]]
    if (!missing(name)) {
      res <- res[[name]]
    }
  }
  
  res
}
#' @rdname params
#' @export
`params<-` <- function(plate, category, name, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  replace <- 'params'
  if (!missing(category)) {
    replace <- c(replace, category)
    if (!missing(name)) {
      replace <- c(replace, name)
    }
  }
  
  plate[[replace]] <- value
  plate
}


#' Potential droplet clusters for a plate type
#' 
#' Each ddPCR plate type has a specific set of potential clusters the droplets
#' can be assigned to. 
#' 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on plate types.
#' @param plate a ddPCR plate.
#' @return A character vector with the names of the clusters supported by the
#' plate type.
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' new_plate(dir) %>% clusters
#' new_plate(dir, plate_types$fam_positive_pnpp) %>% clusters
#' }
#' @export
clusters <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['clusters']]
}

# Set the clusters of a plate to a given set of clusters. This is an internal
# function because the user should never directly call this function.
`clusters<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['clusters']] <- value
  plate
}
#' Get cluster ID by name
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' # see what cluster names exist and their order
#' clusters(plate)
#' cluster(plate, 'FAILED')
#' cluster(plate, 'EMPTY')
#' }
#' @export
#' @keywords internal
cluster <- function(plate, cluster) {
  res <- plate %>% clusters %>% {which(. == cluster)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find cluster `%s`", cluster))
  }
  res
}
#' Get cluster name by ID
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' # see what cluster names exist and their order
#' clusters(plate)
#' cluster_name(plate, 2)
#' cluster_name(plate, 4)
#' }
#' @keywords internal
#' @export
cluster_name <- function(plate, cluster) {
  cluster %>% as.integer
  if (cluster < 1 || cluster > plate %>% clusters %>% length) {
    err_msg(sprintf("invalid cluster number: %s", cluster))
  }
  plate %>% clusters %>% .[cluster]
}

#' Get unanalyzed cluseter IDs
#' 
#' Get the clusters that have not been considered yet in the analysis. This means
#' the UNDEFINED cluster (since all droplets begin as UNDEFINED) and also all
#' clusters that are defined later than the current cluster. The latter is to
#' ensure that when re-running an analysis step, droplets that were analyzed
#' in a later step will still be considered for analysis.
#' 
#' @param plate A ddPCR plate
#' @param current The current cluster ID, which is used to know what clusters
#' come after
#' @return All clusters that have not yet been analyzed
#' @seealso \code{\link[ddpcr]{cluster}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' unanalyzed_clusters(plate, 3)
#' unanalyzed_clusters(plate, cluster(plate, "OUTLIER"))
#' plate %>% unanalyzed_clusters(cluster(plate, "OUTLIER")) %>% cluster_name(plate, .)
#' }
#' @export
#' @keywords internal
unanalyzed_clusters <- function(plate, current) {
  res <- plate %>% cluster('UNDEFINED')
  if (!missing(current)) {
    if (!current %>% is.numeric) {
      current <- cluster(plate, current)
    }
    res %<>%
      c(seq(current, plate %>% clusters %>% length))
  }
  res
}

#' Analysis steps of a ddPCR plate
#' 
#' Every ddPCR plate type has an ordered set of steps that are run to analyze
#' the data. You can run all the steps with \code{\link[ddpcr]{analyze}} or
#' run the analysis step by step with \code{\link[ddpcr]{next_step}}. The order
#' of the steps in the list is the order in which they are run on the dataset.
#' @param plate a ddPCR plate.
#' @return A named character vector, where every name is the human-readable
#' name of an analysis step, and every value is the name of the function
#' used to perform the step.
#' @seealso \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{next_step}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' new_plate(dir) %>% steps
#' new_plate(dir, plate_types$fam_positive_pnpp) %>% steps
#' }
#' @export
steps <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']]
}
# Set the steps of a plate to a given set of steps. This is an internal
# function because the user should never directly call this function.
`steps<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']] <- value
  plate
}
#' Get step ID by step name
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' # see what step names exist and their order
#' steps(plate)
#' step(plate, 'REMOVE_OUTLIERS')
#' }
#' @export
#' @keywords internal
step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find step `%s`", step))
  }
  res
}
#' Get step name by ID
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' # see what step names exist and their order
#' steps(plate)
#' step_name(plate, 2)
#' }
#' @keywords internal
#' @export
step_name <- function(plate, step) {
  step %<>% as.integer
  if (step < 1 || step > plate %>% steps %>% length) {
    err_msg(sprintf("invalid step number: %s", step))
  }
  plate %>% steps %>% names %>% .[step]
}
#' Ensure the plate's status is at the right step
#' 
#' Before beginning a step, you can check to make sure the plate is currently
#' at most one step behind the current step. If the plate is more than one step
#' behind, an error will be thrown, so that the user will know they need to
#' run the previous steps.
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' status(plate) # current step
#' check_step(plate, 2) # are we ready to start step 2?
#' check_step(plate, 3) # are we ready to start step 3?
#' plate <- next_step(plate)
#' status(plate)
#' check_step(plate, 3) # now are we ready to start step 3?  
#' }
#' @export
#' @keywords internal
check_step <- function(plate, step) {
  exists <- (plate %>% status) >= step - 1
  if (!exists) {
    err_msg("analysis is not at the required step")
  }
}
#' Does a ddPCR plate have a step with this name?
#' @param plate A ddPCR plate
#' @param step A step name
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' steps(plate)
#' has_step(plate, 'REMOVE_FAILURES')
#' has_step(plate, 'NO_SUCH_STEP')
#' }
#' @keywords internal
#' @export
has_step <- function(plate, step) {
  plate %>%
    steps %>%
    names %>%
    {which(. == step)} %>%
    length %>%
    magrittr::equals(1)
}

#' Get/set the X/Y variable (dye name)
#' 
#' By default, the dye visualized along the X axis is HEX and the dye visualized
#' along the Y axis is FAM. You can use these functions to get or set these values
#' if your plate uses different dyes.
#' 
#' The X/Y variables are simply parameters in the plate, which can also be accessed
#' or changed using \code{\link[ddpcr]{params}}. You should use these functions
#' to change the X/Y variable rather than changing the parameters directly.
#' @param plate A ddPCR plate
#' @param value New dye name
#' @return Dye name
#' @seealso \code{\link[ddpcr]{params}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' x_var(plate)
#' x_var(plate) <- "VIC"
#' x_var(plate)
#' }
#' @name x_var
NULL

#' @rdname x_var
#' @export
x_var <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  params(plate, 'GENERAL', 'X_VAR')
}
#' @rdname x_var
#' @export
y_var <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  params(plate, 'GENERAL', 'Y_VAR')
}
#' @rdname x_var
#' @export
`x_var<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  value %<>% make.names
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(x_var(plate), value))
  params(plate, 'GENERAL', 'X_VAR') <- value
  plate
}
#' @rdname x_var
#' @export
`y_var<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  value %<>% make.names
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(y_var(plate), value))
  params(plate, 'GENERAL', 'Y_VAR') <- value
  plate
}
