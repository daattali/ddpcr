border_to_str <- function(border) {
  paste(border %>% as.integer, collapse = ",")
}
str_to_border <- function(str) {
  strsplit(str, ",") %>% unlist %>% as.integer
}

classify_droplets_single <- function(plate, well_id, analysis_method_idx = 2, plot = FALSE) {
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
  analysis_methods <- c(analyze_well_clusters_normals,
                        analyze_well_clusters_density_inflection,
                        analyze_well_clusters_density_minima)
  analysis_method <- analysis_methods[[analysis_method_idx]]
  clusters_data <- analysis_method(plate, well_id, plot)
  clusters_data
}

#' @export
classify_droplets <- function(plate, analysis_method_idx = 2) {
  # Mark all drops in a plate with their corresponding clusters, including
  # undefined clusters for failed wells
  #
  # Side effects:
  
  # get a list containing, for each successful well:
  # - a dataframe with all the drops with their clusters
  # - whether or not there is a mutant drops cluster
  # - any comment by the algorithm
  
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_EMPTY_REMOVED)
  
  tstart <- proc.time()
  
  # ---
  
  well_clusters_info <-
    vapply(wells_success(plate),
           function(x) classify_droplets_single(plate, x, analysis_method_idx),
           list('mt_borders', 'wt_borders', 'cl_borders', 'has_mt_cluster', 'comment')) %>%
    lol_to_df

  data <- plate_data(plate)
  data_env <- environment()
  lapply(wells_success(plate),
         function(well_id){
           well_info <-
             well_clusters_info %>%
             dplyr::filter_(~ well == well_id)
           cl_borders <- well_info[['cl_borders']] %>% str_to_border
           mt_borders <- well_info[['mt_borders']] %>% str_to_border
           wt_borders <- well_info[['wt_borders']] %>% str_to_border
           
           # I'm not doing this using dplyr (mutate) because it's much slower
           # this code is a bit ugly but it's much faster to keep overwriting
           # the data rather than create many small dataframes to merge
           non_empty_idx <-
             data[['well']] == well_id &
             (data[['cluster']] == CLUSTER_UNDEFINED |
              data[['cluster']] > CLUSTER_EMPTY)
           cl_idx <- non_empty_idx & data[['FAM']] %btwn% cl_borders
           mt_idx <- cl_idx & data[['HEX']] %btwn% mt_borders
           wt_idx <- cl_idx & data[['HEX']] %btwn% wt_borders
           data[non_empty_idx, 'cluster'] <- CLUSTER_RAIN
           data[mt_idx, 'cluster'] <- CLUSTER_MT
           data[wt_idx, 'cluster'] <- CLUSTER_WT
           assign("data", data, envir = data_env)
           
           NULL
         }
  ) %>% invisible  

  # add metadata (comment/hasMTclust) to each well
  meta <-
    plate_meta(plate) %>%
    merge_dfs_overwrite_col(well_clusters_info,
                            setdiff(names(well_clusters_info), "well"))
  
  # ---
  
  plate_data(plate) <- data
  plate_meta(plate) <- meta
  
  plate %<>% calculate_mt_freqs
  
  status(plate) <- STATUS_DROPLETS_CLASSIFIED
  
  tend <- proc.time()
  message(sprintf("Time to classify droplet clusters: %s seconds",
                  round(tend-tstart)[1]))
  
  plate
}