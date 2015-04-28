parent_plate_type.kras <- function(plate) {
  "ppnp_assay"
}

define_params.kras <- function(plate) {
  params <- NextMethod("define_params")
  
  params[['GENERAL']][['X_VAR']] <- "HEXXX"
  params[['GENERAL']][['Y_VAR']] <- "FAMMM"
  params[['GENERAL']][['POSITIVE_NAME']] <- 'wildtype'
  params[['GENERAL']][['NEGATIVE_NAME']] <- 'mutant'
  params[['GENERAL']][['POSITIVE_DIMENSION']] <- 'X'
  
  params
}