empty_plate <- function() {
  list(
    plate_data = NULL,
    plate_meta = NULL,
    name = NULL,
    status = STATUS_UNDEFINED,
    params = NULL
  )
}

#' @export
new_plate <- function(dir, type = "wtnegbraf", data_files, meta_file, name) {
  plate <- empty_plate()
  
  plate <- set_type(plate, type)
  params(plate) <- default_params(plate)
  
  plate <- read_plate(plate, dir, data_files, meta_file)
  
  if (!missing(name)) {
    name(plate) <- name
  }
  
  plate
}

set_type <- function(plate, type) {
  if (!missing(type) && is.null(type)) {
    return(plate)
  }
  
  if (missing(type)) {
    type <- NULL
  }  
  
  new_class <- type
  if (class(plate)[1] != "list") {
    new_class <- c(class(plate), new_class)
  }
  class(plate) <- new_class
  
  set_type(plate, parent_assay(structure(plate, class = type)))
}

parent_assay <- function(plate) {
  UseMethod("parent_assay")
}

parent_assay.ddpcr_plate <- function(plate) {
  NULL
}

parent_assay.default <- function(plate) {
  "ddpcr_plate"
}

parent_assay.wtnegbraf <- function(plate) {
  "ppnp_assay"
}

default_params <- function(plate) {
  UseMethod("default_params")
}

default_params.ddpcr_plate <- function(plate) {
  DEFAULT_PARAMS
}

default_params.wtnegbraf <- function(plate) {
  params <- NextMethod("default_params")
  params[['general']][['type']] <- "sdfsd"
  params
}

#' @export
plate_meta <- function(x, only_used = FALSE) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  
  if (only_used) {
    x[['plate_meta']] %>% dplyr::filter_(quote(used))
  } else {  
    x[['plate_meta']]
  }
}
`plate_meta<-` <- function(x, value) {
  x[['plate_meta']] <- value
  x
}

well_info <- function(x, well_id, var) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  result <- 
    plate_meta(x) %>%
    dplyr::filter_(~ well == well_id) %>%
    .[[var]]
  result
}

arrange_meta <- function(x) {
  if ("success" %in% colnames(x)) {
    x %>% dplyr::arrange_(~ desc(used), ~ desc(success), ~ row, ~ col)  
  } else {
    x %>% dplyr::arrange_(~ desc(used), ~ row, ~ col)
  }
}

#' @export
plate_data <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['plate_data']]
}
`plate_data<-` <- function(x, value) {
  x[['plate_data']] <- value
  x
}

#' @export
status <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['status']]
}
`status<-` <- function(x, value) {
  x[['status']] <- value
  x
}

#' @export
name <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['name']]
}
#' @export
`name<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['name']] <- value
  x
}

#' @export
params <- function(x, major, minor) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  
  res <- x[['params']]
  if (!missing(major)) {
    res <- res[[major]]
    if (!missing(minor)) {
      res <- res[[minor]]
    }
  }
  
  res
}

#' @export
`params<-` <- function(x, major, minor, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  
  replace <- 'params'
  if (!missing(major)) {
    replace <- c(replace, major)
    if (!missing(minor)) {
      replace <- c(replace, minor)
    }
  }

  x[[replace]] <- value
  x
}

#' @export
wells_used <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(x %>% plate_meta, ~ used) %>%
    .[['well']]
}

#' @export
wells_success <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(x %>% plate_meta, ~ success) %>%
    .[['well']]
}

#' @export
wells_failed <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(x %>% plate_meta, ~ !success) %>%
    .[['well']]
}

#' @export
wells_mutant <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(x %>% plate_meta, ~ has_mt_cluster) %>%
    .[['well']]
}

#' @export
wells_wildtype <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(x %>% plate_meta, ~ !has_mt_cluster) %>%
    .[['well']]
}

#' @export
analyze = function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate %<>% remove_failures     # step 1 - remove failed wells
  plate %<>% remove_outliers     # step 2 - remove outlier droplets
  plate %<>% remove_empty        # step 3 - remove empty droplets
  plate %<>% classify_droplets   # step 4 - classify droplets as mutant/wildtype/rain
  plate %<>% reclassify_droplets # step 5 - reanalyze low mutant frequency wells
  
  plate
}

#' @export
print.ddpcr_plate <- function(x, ...) {
  if (x %>% status < STATUS_INIT) {
    cat0("Empty ddPCR plate")
  } else {
    cat0("Dataset name: ", x %>% name, "\n")
    cat0("Analysis status: ", x %>% status, "\n")
    if (x %>% status >= STATUS_INIT) {
      cat0("Data summary: ", 
           x %>% plate_meta %>% .[['used']] %>% sum, " wells, ",
           x %>% plate_data %>% nrow, " drops\n")
    }
    cat0("---\nDrops data:\n")
    cat0(x %>% plate_data %>% str)
    cat0("---\nPlate meta data:\n")
    cat0(x %>% plate_meta %>% str)
  }
}

# pmini <- new_plate("../../data/mini141")
# p141 <- new_plate("../../data/2-26-2014-BRAFWTNEGASSAY-FFPEDNA-CRC-1-41") %>% analyze

#tstart <- proc.time(); a <- Plate$new("2014-06-06_BRAFWTNEGASSAY_FFPEThyroidscrolls")$analyze(); tend <- proc.time(); print(round(tend-tstart)[1]) 
#7-8 seconds