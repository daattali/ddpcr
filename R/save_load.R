normalize_to_rds <- function(file) {
  ifelse(substring(file, nchar(file) - 3) == ".rds",
         file,
         sprintf("%s.rds", file))
}

save_plate <- function(plate, file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  file %<>% normalize_to_rds
  
  #TODO(daattali) convert data to factors to save space?
  object <- list(
    plate_data = plate_data(plate),
    plate_meta = plate_meta(plate),
    outliers = outliers(plate),
    name = name(plate),
    status = status(plate),
    params = params(plate)
  )
  saveRDS(object = object, file = file)
  
  plate
}

load_plate <- function(file) {
  file %<>% normalize_to_rds
  
  # TODO if save converts to factor, then make sure to reuse all the columns/rows here
  object <- readRDS(file = file)
  
  plate <- empty_plate()
  plate_data(plate) <- object[['plate_data']]
  plate_meta(plate) <- object[['plate_meta']]
  outliers(plate) <- object[['outliers']]
  name(plate) <- object[['name']]
  status(plate) <- object[['status']]
  params(plate) <- object[['params']]
  
  plate
}