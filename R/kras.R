KRAS <- "kras"

parent_plate_type.kras <- function(plate) {
  "ppnp_assay"
}

define_params.kras <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'X_VAR' = "HEX",
      'Y_VAR' = "FAM",
      'POSITIVE_NAME' = 'wildtype',
      'NEGATIVE_NAME' = 'mutant',
      'POSITIVE_DIMENSION' = 'x'
    )
  )
  params %<>% modifyList(new_params)
  
  params
}