## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains functions that get/set ddPCR plate object attributes and
# related functions. Many of these functions are exported and should be used
# by users to get/set plate data.

#' Plate type
#' 
#' Get the type of a ddPCR plate. 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for more
#' information on plate types.
#' @param plate A ddPCR plate
#' @param all If \code{FALSE}, show only the most specific plate type; otherwise,
#' show all inherited (implicit) types as well.
#' @return A character vector with the plate type(s).
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

#' @export
plate_data <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['plate_data']]
}
`plate_data<-` <- function(plate, value) {
  plate[['plate_data']] <- value
  plate
}

#' @export
status <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['status']]
}
`status<-` <- function(plate, value) {
  plate[['status']] <- value
  plate
}
is_empty_plate <- function(plate) {
  is.null(status(plate))
}
#' @export
analysis_complete <- function(plate) {
  status(plate) == length(steps(plate))
}

#' @export
plate_meta <- function(plate, only_used = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (only_used) {
    plate[['plate_meta']] %>% dplyr::filter_(quote(used))
  } else {  
    plate[['plate_meta']]
  }
}
`plate_meta<-` <- function(plate, value) {
  plate[['plate_meta']] <- value
  plate
}
arrange_meta <- function(plate) {
  if ("success" %in% colnames(plate)) {
    plate %>% dplyr::arrange_(~ desc(used), ~ desc(success), ~ row, ~ col)  
  } else {
    plate %>% dplyr::arrange_(~ desc(used), ~ row, ~ col)
  }
}

#' @export
name <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['name']]
}
#' @export
`name<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['name']] <- value
  plate
}

#' @export
params <- function(plate, major, minor) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  res <- plate[['params']]
  if (!missing(major)) {
    res <- res[[major]]
    if (!missing(minor)) {
      res <- res[[minor]]
    }
  }
  
  res
}

#' @export
`params<-` <- function(plate, major, minor, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  replace <- 'params'
  if (!missing(major)) {
    replace <- c(replace, major)
    if (!missing(minor)) {
      replace <- c(replace, minor)
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
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' @param plate a ddPCR plate.
#' @return A character vector with the names of the clusters supported by the
#' plate type.
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






cluster <- function(plate, cluster) {
  res <- plate %>% clusters %>% {which(. == cluster)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find cluster `%s`", cluster))
  }
  res
}
#' @export
cluster_name <- function(plate, cluster) {
  cluster %>% as.integer
  if (cluster < 1 || cluster > plate %>% clusters %>% length) {
    err_msg(sprintf("invalid cluster number: %s", cluster))
  }
  plate %>% clusters %>% .[cluster]
}

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





#' @export
steps <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']]
}
`steps<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']] <- value
  plate
}
step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find step `%s`", step))
  }
  res
}
step_name <- function(plate, step) {
  step %<>% as.integer
  if (step < 1 || step > plate %>% steps %>% length) {
    err_msg(sprintf("invalid step number: %s", step))
  }
  plate %>% steps %>% names %>% .[step]
}
check_step <- function(plate, step, must_exist = FALSE) {
  exists <- plate %>% status >= step - 1
  if (must_exist && !exists) {
    err_msg("analysis is not at the required step")
  }
  exists
}
has_step <- function(plate, step) {
  plate %>%
    steps %>%
    names %>%
    {which(. == step)} %>%
    length %>%
    magrittr::equals(1)
}

#' @export
x_var <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  params(plate, 'GENERAL', 'X_VAR')
}
#' @export
y_var <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  params(plate, 'GENERAL', 'Y_VAR')
}
#' @export
`x_var<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  value %<>% make.names
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(x_var(plate), value))
  params(plate, 'GENERAL', 'X_VAR') <- value
  plate
}
#' @export
`y_var<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  value %<>% make.names
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(y_var(plate), value))
  params(plate, 'GENERAL', 'Y_VAR') <- value
  plate
}
