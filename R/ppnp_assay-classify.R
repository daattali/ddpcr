border_to_str <- function(border) {
  paste(border %>% as.integer, collapse = ",")
}
str_to_border <- function(str) {
  strsplit(str, ",") %>% unlist %>% as.integer
}

#' @export
classify_droplets_single <- function(plate, well_id) {
  UseMethod("classify_droplets_single")
}

#' @export
classify_droplets_single.ppnp_assay <- function(plate, well_id) {
  # For a given well, merge the empty drops with the rain/mutant/wildtype drops
  # to result in a data frame containing all drops in a well marked with a cluster.
  #
  # Args:
  #   plot: If true, plot the result (used mainly for development/debugging)
  #
  # Returns:
  #   list:
  #     result: Dataframe with all drops in the given well assigned to clusters
  #     hasMTcluster: TRUE if a significant mutant cluster was found, FALSE otherwise.
  #       Note that TRUE can be very good proxy for saying the well has mutant BRAFV600,
  #       and FALSE is a proxy for saying the well has wild-type BRAFV600
  #     comment: any comment raised by the algorithm, or NA if everything ran smoothly 
  analysis_method <- 
    params(plate, 'ASSIGN_CLUSTERS', 'METHOD') %>%
    sprintf("classify_droplets_%s", .) %>%
    get
  
  clusters_data <- analysis_method(plate, well_id)
  clusters_data
}

#' @export
classify_droplets <- function(plate) {
  UseMethod("classify_droplets")
}

#' @export
classify_droplets <- function(plate) {
  UseMethod("classify_droplets")
}

#' @export
classify_droplets.ppnp_assay <- function(plate) {
  # Mark all drops in a plate with their corresponding clusters, including
  # undefined clusters for failed wells
  #
  # Side effects:
  
  # get a list containing, for each successful well:
  # - a dataframe with all the drops with their clusters
  # - whether or not there is a mutant drops cluster
  # - any comment by the algorithm
  stopifnot(plate %>% status >= STATUS_EMPTY_REMOVED)
  
  tstart <- proc.time()
  
  # ---
  
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)
  
  well_clusters_info <-
    vapply(wells_success(plate),
           function(x) classify_droplets_single(plate, x),
           vector(mode = "list", length = 5)) %>%
    lol_to_df

  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells_success(plate),
         function(well_id){
           well_info <-
             well_clusters_info %>%
             dplyr::filter_(~ well == well_id)
           
           filled_borders <- well_info[['filled_borders']] %>% str_to_border
           negative_borders <- well_info[['negative_borders']] %>% str_to_border
           positive_borders <- well_info[['positive_borders']] %>% str_to_border
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           # this code is a bit ugly but it's much faster to keep overwriting
           # the data rather than create many small dataframes to merge
           classifiable_idx <-
             data[['well']] == well_id &
             (data[['cluster']] == CLUSTER_UNDEFINED |
              data[['cluster']] >= CLUSTER_RAIN)
           filled_idx <- classifiable_idx & data[[positive_var]] %btwn% filled_borders
           negative_idx <- filled_idx & data[[variable_var]] %btwn% negative_borders
           positive_idx <- filled_idx & data[[variable_var]] %btwn% positive_borders
           
           data[classifiable_idx, 'cluster'] <- CLUSTER_RAIN
           data[negative_idx, 'cluster'] <- CLUSTER_NEGATIVE
           data[positive_idx, 'cluster'] <- CLUSTER_POSITIVE
           assign("data", data, envir = data_env)
           
           NULL
         }
  ) %>% invisible  

  # add metadata (comment/hasMTclust) to each well
  meta <-
    plate_meta(plate) %>%
    merge_dfs_overwrite_col(well_clusters_info)  %>%
    magrittr::set_names(lapply(names(.), function(x) meta_var_name(plate, x)))
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_mt_freqs
  
  status(plate) <- STATUS_DROPLETS_CLASSIFIED
  
  tend <- proc.time()
  message(sprintf("Time to classify droplets: %s seconds",
                  round(tend-tstart)[1]))
  
  plate
}