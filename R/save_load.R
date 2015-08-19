## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Save a ddPCR plate
#' 
#' Saves a plate to a file, including all its data, parameters, and current
#' analysis state.  The file can be read back later using 
#' \code{\link[ddpcr]{load_plate}}.  The file is not human-readable - if
#' you want to save the droplets data or the metadata of a plate, then first
#' retrieve the data using \code{\link[ddpcr]{plate_data}} or
#' \code{\link[ddpcr]{plate_meta}} and save it with 
#' \code{\link[utils]{write.csv}}.
#' 
#' @param plate Plate object to save.
#' @param file Name of the file where the plate will be saved.
#' @return The given plate, unchanged.
#' @seealso \code{\link[ddpcr]{load_plate}}
#' @examples
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' save_plate(plate, "myplate")
#' unlink("myplate.rds")
#' @export
save_plate <- function(plate, file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  file %<>% normalize_to_rds
  
  object <- list(
    class      = class(plate),
    plate_data = plate_data(plate),
    plate_meta = plate_meta(plate),
    name       = name(plate),
    status     = status(plate),
    params     = params(plate),
    clusters   = clusters(plate),
    steps      = steps(plate),
    version    = plate[['version']]
  )
  saveRDS(object = object, file = file)
  
  invisible(plate)
}
#' Load a ddPCR plate
#' 
#' Reloads a plate that has been saved with \code{\link[ddpcr]{save_plate}}.
#'  
#' @param file Name of the file where the plate was saved.
#' @return The plate that was saved in the given file.
#' @seealso \code{\link[ddpcr]{save_plate}}
#' @examples
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir)
#' save_plate(plate, "myplate")
#' plate2 <- load_plate("myplate")
#' plate3 <- load_plate("myplate.rds")
#' identical(plate, plate2)
#' identical(plate, plate3)
#' unlink("myplate.rds")
#' @export
load_plate <- function(file) {
  file %<>% normalize_to_rds
  
  plate <- empty_plate()
  tryCatch({
    object <- readRDS(file = file)
    class(plate)       <- object[['class']]
    plate_data(plate)  <- object[['plate_data']]
    plate_meta(plate)  <- object[['plate_meta']]
    name(plate)        <- object[['name']]
    status(plate)      <- object[['status']]
    params(plate)      <- object[['params']]
    clusters(plate)    <- object[['clusters']]
    steps(plate)       <- object[['steps']]
    plate[['version']] <- object[['version']]
  },
  error = function(err) {
    err_msg(paste("The given file is not a valid ddPCR file",
                 "(are you sure it was saved using this program?)"))
  })
  
  plate
}

#' Normalize a file name to .rds suffix
#' @examples
#' normalize_to_rds("somefile")       # somefile.rds
#' normalize_to_rds("somefile.rds")   # somefile.rds
#' normalize_to_rds("somefile.r")     # somefile.r.rds
#' @keywords internal
#' @export
normalize_to_rds <- function(file) {
  ifelse(substring(file, nchar(file) - 3) == ".rds",
         file,
         sprintf("%s.rds", file))
}
