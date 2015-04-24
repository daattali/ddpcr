# for this assay, we know that we don't expect any drops on the bottom right,
# so it's ok to only have an empty cutoff in the y dimension to save time
get_empty_cutoff.wtnegbraf <- function(plate, well_id){
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR') 

  set.seed(SEED)

  # fit two normal distributions in the data along the y dimension
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[Y_var]], k = 2))
  
  # set the Y cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  cutoff_y <-
    (mixmdl_y$mu[smaller_comp_y] +
       params(plate, 'EMPTY', 'CUTOFF_SD') * mixmdl_y$sigma[smaller_comp_y]) %>%
    ceiling %>%
    as.integer
  
  return(list("cutoff_x" = NULL, "cutoff_y" = cutoff_y)) 
}
