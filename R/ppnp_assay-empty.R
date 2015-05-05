# for this assay, we know that we don't expect any drops on the bottom right,
# so it's ok to only have an empty cutoff in the y dimension to save time
get_empty_cutoff.ppnp_assay <- function(plate, well_id){
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  positive_var <- positive_dim_var(plate)
  
  set.seed(SEED)

  # fit two normal distributions in the data along the y dimension
  quiet(
    mixmdl_pos <- mixtools::normalmixEM(well_data[[positive_var]], k = 2))
  
  # set the Y cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_pos <- mixmdl_pos$mu %>% which.min
  cutoff_pos <-
    (mixmdl_pos$mu[smaller_comp_pos] +
       params(plate, 'REMOVE_EMPTY', 'CUTOFF_SD') * mixmdl_pos$sigma[smaller_comp_pos]) %>%
    ceiling %>%
    as.integer
  
  res <- list()
  res[[positive_dim(plate) %>% tolower]] <- cutoff_pos
  res[[variable_dim(plate) %>% tolower]] <- NA
  return(res) 
}
