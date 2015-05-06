## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

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
    params(plate, 'CLASSIFY', 'METHOD') %>%
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
  CURRENT_STEP <- plate %>% step('CLASSIFY')
  plate %>% check_step(CURRENT_STEP, TRUE)  
  step_begin("Classifying droplets")
  
  # ---

  well_clusters_info <-
    vapply(wells_success(plate),
           function(x) classify_droplets_single(plate, x),
           vector(mode = "list", length = 5)) %>%
    lol_to_df %>%
    magrittr::set_names(lapply(names(.), function(x) meta_var_name(plate, x)))

  # add metadata (comment/hasMTclust) to each well
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(well_clusters_info)
  
  plate %<>%
    mark_clusters(plate %>% wells_success) %>%
    calculate_negative_freqs
  
  # ---
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}

mark_clusters <- function(plate, wells) {
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)
  
  CLUSTER_RAIN <- plate %>% cluster('RAIN')
  CLUSTER_POSITIVE <- plate %>% cluster('POSITIVE')
  CLUSTER_NEGATIVE <- plate %>% cluster('NEGATIVE')
  CLUSTERS_UNANALYZED <- unanalyzed_clusters(plate, 'RAIN')
  
  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells,
    function(well_id){
      get_borders <- function(border_type) {
        well_info(plate, well_id, border_type) %>% str_to_border
      }
      filled_borders <- get_borders('filled_borders')
      negative_borders <- get_borders(meta_var_name(plate, 'negative_borders'))
      positive_borders <- get_borders(meta_var_name(plate, 'positive_borders'))
      
      # I'm not doing this using dplyr (mutate) because it's much slower
      # this code is a bit ugly but it's much faster to keep overwriting
      # the data rather than create many small dataframes to merge
      classifiable_idx <-
        data[['well']] == well_id &
        (data[['cluster']] %in% CLUSTERS_UNANALYZED)
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
  
  plate_data(plate) <- data
  plate
}