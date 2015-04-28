# For a given well, calculate the Y cutoff where all drops below this
# value are considered empty
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .plot: If true, plot the result (used mainly for development/debugging)
#
# Returns:
#   The Y value that is the cutoff for empty drops in the well
#
# Algorithm:
#   We fit a mixture of 2 normal populations in the Y values of all the drops.
#   The lower population corresponds to the empty drops, which form a fairly
#   dense cluster. To capture the cutoff of empty drops, we simply assume that
#   the Y intensity of empty drops can be roughly modeled by a normal distribution.
#   More specifically, we set the threshold at 7 (PARAMS$EMPTY$CUTOFF_SD)
#   standard deviations above the center of the distribution.
#' Get the cutoff for empty droplets in a well
#' 
#' @export
#' @keywords internal
get_empty_cutoff <- function(plate, well_id) {
  UseMethod("get_empty_cutoff")
}

#' Algorithm for determining the cutoff for empty droplets in a well
#' 
#' @export
#' @keywords internal
get_empty_cutoff.ddpcr_plate <- function(plate, well_id){
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # fit two normal distributions in the data along the Y dimension
  set.seed(SEED)
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[y_var(plate)]], k = 2))
  
  # set the Y cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  cutoff_y <-
    (mixmdl_y$mu[smaller_comp_y] +
    params(plate, 'EMPTY', 'CUTOFF_SD') * mixmdl_y$sigma[smaller_comp_y]) %>%
    ceiling %>%
    as.integer

  # fit two normal distributions in the data along the x dimension
  set.seed(SEED)
  quiet(
    mixmdl_x <- mixtools::normalmixEM(well_data[[x_var(plate)]], k = 2))
  # set the X cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_x <- mixmdl_x$mu %>% which.min
  cutoff_x <-
    (mixmdl_x$mu[smaller_comp_x] +
       params(plate, 'EMPTY', 'CUTOFF_SD') * mixmdl_x$sigma[smaller_comp_x]) %>%
    ceiling %>%
    as.integer  
  
  return(list("x" = cutoff_x, "y" = cutoff_y)) 
}


# Find the empty drops on the plate and mark them with the empty cluster
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .metadata: The dataframe containing the metadata of plate
#
# Returns:
#   list:
#     result: Dataframe containing all drops, with the empty drops marked with
#       with their cluster
#     cutoffs: The Y cutoff of every well
# Removes outlier drops from a plate
#
# Args:
#   .wellData: The dataframe containing all the droplets
#
# Returns:
#   Dataframe with outliers removed
#' Remove empty droplets
#' @export
remove_empty <- function(plate) {
  UseMethod("remove_empty")
}

#' Remove empty droplets
#' 
#' The algorithm for removing empty droplets from a plate
#' 
#' @export
#' @keywords internal
remove_empty.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step('REMOVE_EMPTY')
  plate %>% check_step(CURRENT_STEP)
  step_begin("Finding empty droplets")
  
  # ---
  
  # get the empty cutoff of every well
  empty_cutoff_map <-
    vapply(wells_success(plate),
           function(x) get_empty_cutoff(plate, x),
           list("x", "y")) %>%
    lol_to_df
  
  # set the cluster to EMPTY for every empty droplet in every well
  data <- plate_data(plate)
  data_env <- environment()
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  CLUSTERS_UNANALYZED <- unanalyzed_clusters(plate, 'EMPTY')
  lapply(empty_cutoff_map[['well']],
         function(well_id){

           cutoff_x <-
             empty_cutoff_map %>%
             dplyr::filter_(~ well == well_id) %>%
             .[['x']]         
           
           cutoff_y <-
             empty_cutoff_map %>%
             dplyr::filter_(~ well == well_id) %>%
             .[['y']]
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           empty_idx <-
             data[['well']] == well_id &
             (data[['cluster']] %in% CLUSTERS_UNANALYZED)
           if (!is.na(cutoff_x)) {
             empty_idx <- empty_idx & data[[x_var]] < cutoff_x
           }
           if (!is.na(cutoff_y)) {
             empty_idx <- empty_idx & data[[y_var]] < cutoff_y
           }
           
           # this is a bit ugly but it's much faster to keep overwriting the
           # data rather than create many small dataframes and then merging/
           # overwriting with the original data
           data[empty_idx, 'cluster'] <- plate %>% cluster('EMPTY')
           assign("data", data, envir = data_env)
           
           NULL
         }
  ) %>% invisible
  
  meta <-
    data %>%
    dplyr::filter_(~ cluster == plate %>% cluster('EMPTY')) %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops_empty" = ~ n()) %>%
    merge_dfs_overwrite_col(plate_meta(plate), ., "drops_empty") %>%
    dplyr::mutate_(.dots = setNames(
      list(
        lazyeval::interp(~ ifelse(is.na(empty), NA, drops - empty),
                         empty = quote(drops_empty), drops = quote(drops)),
        lazyeval::interp(~ ifelse(is.na(empty), NA, signif(empty / drops, 3)),
                         empty = quote(drops_empty), drops = quote(drops))
      ),
      c("drops_non_empty", "drops_empty_fraction")
    ))
    
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_concentration
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}

