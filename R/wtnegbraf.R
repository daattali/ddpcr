WTNEGBRAF <- "wtnegbraf"

parent_plate_type.wtnegbraf <- function(plate) {
  "mutant_wildtype_assay"
}

define_params.wtnegbraf <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'X_VAR'              = 'HEX',
      'Y_VAR'              = 'FAM',
      'POSITIVE_DIMENSION' = 'Y'
    )
  )
  params %<>% modifyList(new_params)
    
  params
}

