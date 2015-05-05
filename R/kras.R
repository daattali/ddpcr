KRAS <- "kras"

parent_plate_type.kras <- function(plate) {
  "mutant_wildtype_assay"
}

define_params.kras <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'X_VAR' = "HEX",
      'Y_VAR' = "FAM",
      'POSITIVE_DIMENSION' = 'X'
    )
  )
  params %<>% modifyList(new_params)
  
  params
}