empty_plate <- function() {
  structure(
    list(
      plate_data = NULL,
      plate_meta = NULL,
      name = NULL,
      status = STATUS_INIT,
      params = DEFAULT_PARAMS
    ),
    class = "ddpcr_plate"
  )
}

#' @export
plate <- function(dir, data_files, meta_file, name) {
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

#' @export
wells_success <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(plateMeta(x), ~ success) %>%
    .[['well']]
}

#' @export
wells_failed <- function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  dplyr::filter_(plateMeta(x), ~ !success) %>%
    .[['well']]
}

#' @export
analyze = function(x) {
  stopifnot(x %>% inherits("ddpcr_plate"))
  
  #self$loadData()         # step 0 - load data
  #self$removeOutliers()   # step 1 - remove outlier droplets
  #self$removeFailures()   # step 2 - remove failed wells
  #self$markEmpty()        # step 3 - remove empty droplets
  #self$classifyDroplets() # step 4 - classify droplets as mutant/wildtype/rain
  #self$reclassifyLowMt()  # step 5 - reanalyze low mutant frequency wells
}

#' @export
print.ddpcr_plate <- function(x, ...) {
  cat("Dataset name: ", x %>% name, "\n", sep = "")
  cat("Analysis status: ", x %>% status, "\n", sep = "")
  cat("Drops data:\n", sep = "")
  cat(x %>% plate_data %>% str)
  cat("Plate meta data:\n", sep = "")
  cat(x %>% plate_meta %>% str)
}

# dhorizon <- Plate$new("horizon")$analyze()
# dhorizon2 <- Plate$new(p = dhorizon)
# dhorizon3 <- Plate$new()$load("inst/sampledata/dhorizon")
# dthy <- Plate$new()$load("inst/sampledata/thy")
# mini <- loadPlate("inst/sampledata/mini141")

#tstart <- proc.time(); a <- Plate$new("2014-06-06_BRAFWTNEGASSAY_FFPEThyroidscrolls")$analyze(); tend <- proc.time(); print(round(tend-tstart)[1]) 
#7-8 seconds