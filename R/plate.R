empty_plate <- function() {
  list(
    plate_data = NULL,
    plate_meta = NULL,
    name = NULL,
    status = NULL,
    params = NULL,
    clusters = NULL,
    steps = NULL
  )
}

init_plate <- function(type) {
  # Unfortunately this pipeline can't be piped with %>% because magrittr
  # doesn't seem to play nicely with missing arguments  
  plate <- empty_plate()  # Start with a new empty plate
  plate <- set_plate_type(plate, type)  # Set the type (class) of this assay
  plate <- set_default_clusters(plate)
  plate <- set_default_steps(plate)
  plate <- set_default_params(plate)   # Set the right parameters based on the plate type
  status(plate) <- plate %>% step('INIT')
  plate
}

#' @export
new_plate <- function(dir, type, data_files, meta_file, name) {
  plate <- init_plate(type)
  plate <- read_plate(plate, dir, data_files, meta_file)  # Read the data files into the plate

  # If a name was given, use it instead of the automatically extracted
  if (!missing(name)) {
    name(plate) <- name
  }
  
  plate
}

# Set the type of plate. Supports multi-level inheritance
# All types are by default ddpcr_plate, but users can define new assay types
# that inherit from another one (which means they will use the same params/
# methods)
set_plate_type <- function(plate, type) {
  if (!missing(type) && is.null(type)) {
    return(plate)
  }
  
  if (missing(type)) {
    type <- NULL
  }  
  
  # Add the given type to the classlist (initially the class will be "list"
  # because that's the mode of a plate - we want to exclude that one)
  new_class <- type
  if (class(plate)[1] != "list") {
    new_class <- c(class(plate), new_class)
  }
  class(plate) <- new_class
  
  # Recursively add the plate type of the parent
  set_plate_type(plate,
                 structure(plate, class = type) %>% parent_plate_type)
}

# Each plate type can define a "parent" plate type
# ddpcr_plate is the last parent of any plate type and has no parent itself
parent_plate_type <- function(plate) {
  UseMethod("parent_plate_type")
}
parent_plate_type.ddpcr_plate <- function(plate) {
  NULL
}
parent_plate_type.default <- function(plate) {
  "ddpcr_plate"
}

#' @export
set_default_params <- function(plate) {
  params(plate) <- define_params(plate)
  plate
}

# Each plate type can define its own parameters
define_params <- function(plate) {
  UseMethod("define_params")
}
define_params.ddpcr_plate <- function(plate) {
  DEFAULT_PARAMS
}

set_default_clusters <- function(plate) {
  clusters(plate) <- define_clusters(plate)
  plate
}

define_clusters <- function(plate) {
  UseMethod("define_clusters")
}
clusters <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['clusters']]
}

`clusters<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['clusters']] <- value
  plate
}


set_default_steps <- function(plate) {
  steps(plate) <- define_steps(plate)
  plate
}

define_steps <- function(plate) {
  UseMethod("define_steps")
}
steps <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']]
}

`steps<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['steps']] <- value
  plate
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

well_info <- function(plate, well_id, var) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate_meta(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    .[[var]]
}

arrange_meta <- function(plate) {
  if ("success" %in% colnames(plate)) {
    plate %>% dplyr::arrange_(~ desc(used), ~ desc(success), ~ row, ~ col)  
  } else {
    plate %>% dplyr::arrange_(~ desc(used), ~ row, ~ col)
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

#' @export
wells_used <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ used) %>%
    .[['well']]
}

#' @export
wells_success <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return(NULL)
  }
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ success) %>%
    .[['well']]
}

#' @export
wells_failed <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return(NULL)
  }  
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ !success) %>%
    .[['well']]
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
  
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(x_var(plate), value))
  params(plate, 'GENERAL', 'X_VAR') <- value
  plate
}
#' @export
`y_var<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate_data(plate) %<>%
    dplyr::rename_(.dots = setNames(y_var(plate), value))
  params(plate, 'GENERAL', 'Y_VAR') <- value
  plate
}

#' @export
analyze <- function(plate, restart = FALSE) {
  if (restart) {
    message("Restarting analysis")
    status(plate) <- 0
  }
  steps_left <- length(steps(plate)) - status(plate)
  next_step(plate, n = steps_left)
  message("Analysis complete")
}

analysis_complete <- function(plate) {
  status(plate) == length(steps(plate))
}

#' @export
next_step <- function(plate, n = 1) {
  if (n == 0) {
    return(plate)
  }
  
  if (analysis_complete(plate)) {
    message("Analysis complete")
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

step_begin <- function(text) {
  .globals$set("step_tstart", proc.time())
  message(text, "... ", appendLF = FALSE)
}
step_end <- function(time) {
  message(sprintf("DONE (%s seconds)",
                  round(proc.time() - .globals$get("step_tstart"))[1]))
}

#' @export
print.ddpcr_plate <- function(x, ...) {
  cat0("Dataset name: ", x %>% name, "\n")
  cat0("Plate type: ", x %>% class %>% paste(collapse = ", "), "\n")
  if (analysis_complete(plate)) {
    cat0("Analysis completed")
  } else {
    cat0("Completed analysis steps: ",
         step_name(plate, seq(1, status(plate))) %>% paste(collapse = ", "),
         "\n"
    )
    cat0("Remaining analysis steps: ",
         step_name(plate, seq(status(plate) + 1, length(steps(plate)))) %>% paste(collapse = ", "),
         "\n"
    )
  }
  cat0("Data summary: ", 
       x %>% plate_meta %>% .[['used']] %>% sum, " wells, ",
       x %>% plate_data %>% nrow, " drops\n")
  cat0("---\nDrops data:\n")
  cat0(x %>% plate_data %>% str)
  cat0("---\nPlate meta data:\n")
  cat0(x %>% plate_meta %>% str)
}

# pmini <- new_plate("../../data/mini141")
# p141 <- new_plate("../../data/2-26-2014-BRAFWTNEGASSAY-FFPEDNA-CRC-1-41") %>% analyze

#tstart <- proc.time(); a <- Plate$new("2014-06-06_BRAFWTNEGASSAY_FFPEThyroidscrolls")$analyze(); tend <- proc.time(); print(round(tend-tstart)[1]) 
#7-8 seconds