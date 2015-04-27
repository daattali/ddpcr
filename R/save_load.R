normalize_to_rds <- function(file) {
  ifelse(substring(file, nchar(file) - 3) == ".rds",
         file,
         sprintf("%s.rds", file))
}

save_plate <- function(plate, file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  file %<>% normalize_to_rds
  
  object <- list(
    class = class(plate),
    plate_data = plate_data(plate),
    plate_meta = plate_meta(plate),
    name = name(plate),
    status = status(plate),
    params = params(plate),
    enums = enums(plate)
  )
  saveRDS(object = object, file = file)
  
  invisible(plate)
}

load_plate <- function(file) {
  file %<>% normalize_to_rds
  
  object <- readRDS(file = file)
  
  plate <- empty_plate()
  class(plate) <- object[['class']]
  plate_data(plate) <- object[['plate_data']]
  plate_meta(plate) <- object[['plate_meta']]
  name(plate) <- object[['name']]
  status(plate) <- object[['status']]
  params(plate) <- object[['params']]
  enums(plate) <- object[['enums']]
  
  plate
}