## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

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

setup_new_plate <- function(type) {
  plate <- empty_plate()  # Start with a new empty plate
  setup_plate(plate, type)
}

setup_plate <- function(plate, type) {
  plate <- set_plate_type(plate, type)  # Set the type (class) of this assay
  plate <- set_default_params(plate)
  plate
}

type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}

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
    params(plate) %<>% modifyList(params)
  }
  plate
}

init_plate <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  step_begin(sprintf("Initializing plate of type `%s`", type(plate)))
  
  plate %<>%
    set_default_clusters %>%
    set_default_steps %>%
    init_data %>%
    init_meta
  
  status(plate) <- step(plate, 'INITIALIZE')
  step_end()
  
  plate
}

init_data <- function(plate) {
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  plate_data(plate) %<>%
    magrittr::set_colnames(c("well", x_var, y_var)) %>%
      dplyr::select_("well", x_var, y_var) %>%
      dplyr::mutate_(.dots = setNames(
        list(~ cluster(plate, 'UNDEFINED'),
             lazyeval::interp(~ as.integer(var), var = as.name(x_var)),
             lazyeval::interp(~ as.integer(var), var = as.name(y_var))
        ),
        c("cluster", x_var, y_var))) %>%
      dplyr::arrange_(~ well)  # arrange by wells alphabetically 
  plate
}

init_meta <- function(plate) {
  plate_data <- plate_data(plate)
  plate_meta <- plate_meta(plate)
  
  # read the meta data file (we only care about the well -> sample mapping)
  if (is.null(plate_meta)) {
    plate_meta <- DEFAULT_PLATE_META
  } else {
    meta_cols_keep <- c("well", "sample")
    plate_meta %<>%
      dplyr::select_(~ one_of(meta_cols_keep)) %>%
      unique %>%
      merge_dfs_overwrite_col(DEFAULT_PLATE_META, ., "sample", "well")
  }
  
  # populate the metadata with some initial variables
  wells_used <- plate_data[['well']] %>% unique
  plate_meta[['used']] <- plate_meta[['well']] %in% wells_used
  plate_meta <-
    plate_data %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops" = ~ n()) %>%
    dplyr::left_join(plate_meta, ., by = "well") %>%
    arrange_meta
  
  plate_meta(plate) <- plate_meta
  plate
}

#' @export
new_plate <- function(dir, type, data_files, meta_file, name, params) {
  plate <- setup_new_plate(type)
  plate <- read_plate(plate, dir, data_files, meta_file)  # Read the data files into the plate

  # If a name was given, use it instead of the automatically extracted
  if (!missing(name)) {
    name(plate) <- name
  }
  
  if (!missing(params)) {
    params(plate) %<>% modifyList(params)
  }
  
  plate <- init_plate(plate)
  
  plate
}

# Set the type of plate. Supports multi-level inheritance
# All types are by default ddpcr_plate, but users can define new assay types
# that inherit from another one (which means they will use the same params/
# methods)
set_plate_type <- function(plate, type) {
  if (!missing(type) && is.null(type) && class(plate) != "list") {
    return(plate)
  }
  
  if (missing(type) || is.null(type) || !nzchar(type)) {
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
  # Each parameter has a somewhat descriptive name of what it is used for, and
  # all parameters used by a single step in the pipeline are in a list together
  PARAMS_GENERAL <- list()
  PARAMS_GENERAL['X_VAR'] <- "Channel_2"
  PARAMS_GENERAL['Y_VAR'] <- "Channel_1"
  PARAMS_GENERAL['DROPLET_VOLUME'] <- 0.91e-3
  PARAMS_REMOVE_OUTLIERS <- list()
  PARAMS_REMOVE_OUTLIERS['TOP_PERCENT'] <- 1
  PARAMS_REMOVE_OUTLIERS['CUTOFF_IQR'] <- 5
  PARAMS_REMOVE_FAILURES <- list()
  PARAMS_REMOVE_FAILURES['TOTAL_DROPS_T'] <- 5000
  PARAMS_REMOVE_FAILURES['NORMAL_LAMBDA_LOW_T'] <- 0.3
  PARAMS_REMOVE_FAILURES['NORMAL_LAMBDA_HIGH_T'] <- 0.99
  PARAMS_REMOVE_FAILURES['NORMAL_SIGMA_T'] <- 200
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
has_step <- function(plate, step) {
  plate %>%
    steps %>%
    names %>%
    {which(. == step)} %>%
    length %>%
    magrittr::equals(1)
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
  
  if (!(plate %>% has_step('REMOVE_FAILURES')) ||
      plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return(plate %>% plate_meta %>% .[['well']])
  }
  plate %>%
    plate_meta %>%
    dplyr::filter_(~ success) %>%
    .[['well']]
}

#' @export
wells_failed <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!(plate %>% has_step('REMOVE_FAILURES')) ||
      plate %>% status < step(plate, 'REMOVE_FAILURES')) {
    return(c())
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
  plate %<>% next_step(n = steps_left)
  message("Analysis complete")
  plate
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
  if (is.null(status(x))) {
    cat0("Empty ddPCR plate")
    return()
  }
  
  cat0("Dataset name: ", x %>% name, "\n")
  cat0("Plate type: ", x %>% type(TRUE) %>% paste(collapse = ", "), "\n")
  if (analysis_complete(x)) {
    cat0("Analysis completed\n")
  } else {
    cat0("Completed analysis steps: ",
         step_name(x, seq(1, status(x))) %>% paste(collapse = ", "),
         "\n"
    )
    cat0("Remaining analysis steps: ",
         step_name(x, seq(status(x) + 1, length(steps(x)))) %>% paste(collapse = ", "),
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