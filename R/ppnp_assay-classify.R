## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' @export
classify_droplets_single <- function(plate, well_id) {
  UseMethod("classify_droplets_single")
}

#' @export
classify_droplets_single.ppnp_assay <- function(plate, well_id, plot = FALSE) {
  # For a given well, merge the empty drops with the rain/mutant/wildtype drops
  # to result in a data frame containing all drops in a well marked with a cluster.
  #
  # Args:
  #   plot: If true, plot the result (used mainly for development/debugging
  stopifnot(plate %>% inherits("ppnp_assay"))
  
  signif_negative_cluster <- FALSE
  msg <- NA  
  well_data <- get_single_well(plate, well_id)
  
  variable_var <- variable_dim_var(plate)
  
  filled_borders <- get_filled_borders(plate, well_id)
  filled <- get_filled_drops(plate, well_id, filled_borders)
  
  # whether or not the result is found using the first attempted bandwidth
  bw_orig <- TRUE
  # previous "adjust" value
  adj_prev <- NULL 
  # final "adjust" value
  adj_final <- NULL 
  for (adj in seq(params(plate, 'CLASSIFY', 'ADJUST_BW_MIN'),
                  params(plate, 'CLASSIFY', 'ADJUST_BW_MAX'),
                  0.5)) {
    dens_smooth <- density(filled[[variable_var]], bw = "sj", adjust = adj)
    maxima_idx <- local_maxima(dens_smooth$y)
    minima_idx <- local_minima(dens_smooth$y)
    
    # ensure the right peak is centered on the wildype cluster and not a couple
    # outlier drops
    while (TRUE) {
      btwn_right_mins <-
        filled %>%
        dplyr::filter_(lazyeval::interp(~ var %btwn% dens_smooth$x[tail(minima_idx, 2)],
                                        var = as.name(variable_var)))
      
      if (nrow(btwn_right_mins) < nrow(filled) * 0.1) {
        # discard rightmost extreme points
        maxima_idx %<>% head(-1)
        minima_idx %<>% head(-1)
        msg <- sprintf("ignored small cluster of drops above the %s cluster",
                       params(plate, 'GENERAL', 'POSITIVE_NAME'))
      } else {
        break
      }
    }
    
    num_peaks <- length(maxima_idx)
    if (num_peaks == 1) {
      if (!bw_orig) {
        adj <- adj_prev
        dens_smooth <- density(filled[[variable_var]], bw = "sj", adjust = adj)
        maxima_idx <- local_maxima(dens_smooth$y)
        minima_idx <- local_minima(dens_smooth$y)
        num_peaks <- length(maxima_idx)
      }
      adj_final <- adj
      break
    }
    if (num_peaks == 2) {
      adj_final <- adj
      break
    }
    adj_prev <- adj
    bw_orig <- FALSE
  }
  if (is.null(adj_final)) {
    err_msg(sprintf("Could not analyze well %s", well_id))
  }
  
  right_border <- 0
  if (num_peaks > 1) {
    # use the local minimum to the right of the left-most peak
    left_peak <- dens_smooth$x[maxima_idx][1]
    minimas <- dens_smooth$x[minima_idx]
    right_border <- minimas[which(minimas > left_peak) %>% min]
  }
  
  negative_borders <- c(0, right_border)
  positive_borders <- c(negative_borders[2] + 1, max(filled[[variable_var]]))
  negative_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% negative_borders,
                                    var = as.name(variable_var)))    
  positive_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                    var = as.name(variable_var)))   
  
  # to try to replicate the human approach, we want to get the mutant border as
  # close as possible to the mutant cluster rather than in the middle between
  # mutants and wildtypes
  negative_border_tight <-
    mean(negative_drops[[variable_var]]) +
    (sd(negative_drops[[variable_var]]) * 3)
  if (!is.na(negative_border_tight) && negative_border_tight < right_border) {
    negative_borders[2] <- negative_border_tight
    positive_borders[1] <- negative_borders[2] + 1
    negative_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var %btwn% negative_borders,
                                      var = as.name(variable_var)))       
    positive_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                      var = as.name(variable_var))) 
  }  
  
  negative_freq <- calc_negative_freq_simple(nrow(negative_drops), nrow(positive_drops))
  
  if (plot) {
    graphics::plot(
      well_data,
      main = sprintf("%s (%s%%)\nadj=%s (bw: %s)", well_id, negative_freq, adj_final,
                     round(dens_smooth$bw)),
      xlab = paste0("max: ", paste(round(dens_smooth$x[maxima_idx]), collapse = ", "),
                    "\nmin: ", paste(round(dens_smooth$x[minima_idx]), collapse = ", ")))
    points(filled, col = "blue")
    points(negative_drops, col = "purple3")
    points(positive_drops, col = "green3")
    abline(h = filled_borders %>% mean, col = "grey")
    abline(h = filled_borders, col = "black")
    abline(v = dens_smooth$x[minima_idx])
    abline(v = dens_smooth$x[maxima_idx], col = "grey")
    abline(v = negative_border_tight, col = "red")
  }

  # detemine if the well has a statistically significant negative cluster 
  signif_negative_cluster <-
    has_signif_negative_cluster(plate, nrow(negative_drops), nrow(positive_drops))
  
  return(list(
    'negative_borders' = negative_borders %>% border_to_str,
    'positive_borders' = positive_borders %>% border_to_str,
    'filled_borders'   = filled_borders %>% border_to_str,
    'significant_negative_cluster' = signif_negative_cluster,
    'comment'          = msg
  ))
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

#' Classify a well as having a significant negative cluster (eg. a mutant well)
#' or not using a binomial test.
#' We can call a well as mutant if it is statistically significantly more then
#' 1% with a p-val < 0.01. For example, if there are 500 total drops and 7
#' mutant drops, then the mutant frequency is 1.4%, but is it statistically
#' significantly more than 1%?
#' P(x >= 7)
#'   = 1 - P(x <= 7) + P(x = 7)
#'   = 1 - pbinom(7, 500, .01) + dbinom(7, 500, .01)
#'   = 0.237
#'   > 0.01
#' So not statistically significantly enough, so we say it's a wildtype well.
#' But if there are 5000 drops and 70 mutant drops (same 1.4% frequency but
#' with higher absolute numbers), then
#' P(x >= 70) = 1 - pbinom(70, 5000, .01) + dbinom(70, 5000, .01) = 0.004
#' So this is indeed significant, and this well would be deemed mutant.
has_signif_negative_cluster <- function(plate, neg, pos) {
  freq_threshold <- params(plate, 'CLASSIFY', 'SIGNIFICANT_NEGATIVE_FREQ')
  pval <- params(plate, 'CLASSIFY', 'SIGNIFICANT_P_VALUE')
  total <- neg + pos
  result <- 
    (1 - pbinom(neg, total, freq_threshold) + dbinom(neg, total, freq_threshold)) < pval
  result
}