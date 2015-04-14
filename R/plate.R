empty_plate <- function() {
  structure(
    list(
      plate_data = NULL,
      plate_meta = NULL,
      outliers = NULL,
      name = NULL,
      status = STATUS_UNDEFINED,
      params = DEFAULT_PARAMS
    ),
    class = "ddpcr_plate"
  )
}

#' @export
new_plate <- function(dir, data_files, meta_file, name) {
  plate <- empty_plate()
  plate <- read_plate(plate, dir, data_files, meta_file)
  
  if (!missing(name)) {
    name(plate) <- name
  }
  
  plate
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
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['plate_meta']] <- value
  x
}

#' @export
plate_data <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['plate_data']]
}
`plate_data<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['plate_data']] <- value
  x
}

#' @export
outliers <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['outliers']]
}
`outliers<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['outliers']] <- value
  x
}

#' @export
status <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['status']]
}
`status<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['status']] <- value
  x
}

#' @export
name <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['name']]
}
`name<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['name']] <- value
  x
}

#' @export
params <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['params']]
}
`params<-` <- function(x, value) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  x[['params']] <- value
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
analyze = function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  plate %<>% remove_outliers    # step 1 - remove outlier droplets
  #plate %<>% remove_failures    # step 2 - remove failed wells
  #self$markEmpty()        # step 3 - remove empty droplets
  #self$classifyDroplets() # step 4 - classify droplets as mutant/wildtype/rain
  #self$reclassifyLowMt()  # step 5 - reanalyze low mutant frequency wells
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
# p141 <- new_plate("../../data/2-26-2014-BRAFWTNEGASSAY-FFPEDNA-CRC-1-41")

#tstart <- proc.time(); a <- Plate$new("2014-06-06_BRAFWTNEGASSAY_FFPEThyroidscrolls")$analyze(); tend <- proc.time(); print(round(tend-tstart)[1]) 
#7-8 seconds