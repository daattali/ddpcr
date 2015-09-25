## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Analysis step: Classify droplets
#' 
#' The main analysis step for ddPCR plates of type \code{pnpp_experiment}.
#' Classify each droplet as either rain, ++, or +-. Also calculate the frequency
#' of negative droplets, and attempt to detemine if each well has a statistically
#' significant number of such droplets.\cr\cr
#' \href{https://github.com/daattali/ddpcr#advanced-topic-2-algorithms-used-in-each-step}{See the README} for
#' more information about the algorithm used.
#' 
#' This function is recommended to be run as part of an analysis pipeline (ie.
#' within the \code{\link[ddpcr]{analyze}} function) rather than being called
#' directly.
#' 
#' @param plate A ddPCR plate.
#' @return A ddPCR plate with all droplets assigned to a cluster. The plate's
#' metadata will have several new variables.
#' @seealso \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{classify_droplets_single}}\cr
#' \code{\link[ddpcr]{mark_clusters}}\cr
#' \code{\link[ddpcr]{has_signif_negative_cluster}}
#' 
#' @note This is an S3 generic, which means that different ddPCR plate types can
#' implement this function differently. 
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on how to implement custom ddPCR plate types.
#' @export
#' @keywords internal
classify_droplets <- function(plate) {
  UseMethod("classify_droplets")
}

#' Analysis step: Classify droplets
#' @inheritParams classify_droplets
#' @export
#' @keywords internal
classify_droplets.pnpp_experiment <- function(plate) {
  CURRENT_STEP <- plate %>% step('CLASSIFY')
  plate %>% check_step(CURRENT_STEP)  
  step_begin("Classifying droplets")
  
  # ---
  
  # get droplet classifications in each well
  well_clusters_info <-
    vapply(wells_success(plate),
           function(x) classify_droplets_single(plate, x),
           vector(mode = "list", length = 3)) %>%
    lol_to_df %>%
    magrittr::set_names(lapply(names(.), function(x) meta_var_name(plate, x)))
  
  # add metadata to each well
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(well_clusters_info)
  
  # mark the droplets with their assigned cluster in the plate and calculate frequencies
  plate %<>%
    mark_clusters(plate %>% wells_success) %>%
    calculate_negative_freqs
  
  # ---
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}

#' Classify droplets in a well 
#' 
#' This function runs the actual algorithm for classifying droplets in a single
#' well. 
#' @keywords internal
#' @export
classify_droplets_single <- function(plate, well_id, ...) {
  UseMethod("classify_droplets_single")
}

#' Classify droplets in a well 
#' 
#' If you want to see details about how a well was classified, you can set
#' \code{plot = TRUE} to plot the results.
#' @export
#' @keywords internal
classify_droplets_single.pnpp_experiment <- function(plate, well_id, ..., plot = FALSE) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  
  signif_negative_cluster <- FALSE
  well_data <- get_single_well(plate, well_id)
  variable_var <- variable_dim_var(plate)

  # get the filled borders (threshold under which all droplets are considered rain)
  filled_border <- get_filled_border(plate, well_id)
  filled <- get_filled_drops(plate, well_id, filled_border)
  
  # whether or not the result is found using the first attempted bandwidth
  bw_orig <- TRUE
  # previous "adjust" value
  adj_prev <- NULL 
  # final "adjust" value
  adj_final <- NULL 
  
  # try to find the optimal bandwidth for the density kernel
  for (adj in seq(params(plate, 'CLASSIFY', 'ADJUST_BW_MIN'),
                  params(plate, 'CLASSIFY', 'ADJUST_BW_MAX'),
                  0.5)) {
    dens_smooth <- density(filled[[variable_var]], bw = "sj", adjust = adj)
    maxima_idx <- local_maxima(dens_smooth$y)
    minima_idx <- local_minima(dens_smooth$y)
    
    # ensure the right peak is centered on the wildype cluster and not a few
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
      } else {
        break
      }
    }
    
    # number of peaks = number of local maxima
    num_peaks <- length(maxima_idx)
    
    # if there is only one peak, record the bandwidth and stop
    if (num_peaks == 1) {
      # if it's not the first attempt, revert to the previous bandwidth
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
    # if there are two peaks, stop
    if (num_peaks == 2) {
      adj_final <- adj
      break
    }
    
    # if there are more than 2 peaks, increase the bandwidth
    adj_prev <- adj
    bw_orig <- FALSE
  }
  if (is.null(adj_final)) {
    warn_msg(paste0("Could not accurately analyze well ", well_id, 
                    ", please double-check the results to ensure it looks ok"))
  }
  
  # if there is only one peak, then everything is positive
  # otherwise, use the local minimum to the right of the left-most peak
  # usually there will only be two peaks, so this means the only local minimum.
  # But if we failed to find a bandwidth that produces only two peaks, then
  # we need to make sure we choose the correct minimum
  negative_border <- 0
  if (num_peaks > 1) {
    left_peak <- dens_smooth$x[maxima_idx][1]
    minimas <- dens_smooth$x[minima_idx]
    negative_border <- minimas[which(minimas > left_peak) %>% min]
  }
  
  # mark all the negative and positive droplets according to the border
  negative_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var <= negative_border,
                                    var = as.name(variable_var)))    
  positive_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var > negative_border,
                                    var = as.name(variable_var)))   
  
  # to try to replicate the human approach, we want to get the mutant border as
  # close as possible to the mutant cluster rather than in the middle between
  # mutants and wildtypes
  negative_border_tight <-
    mean(negative_drops[[variable_var]]) +
    (sd(negative_drops[[variable_var]]) * 3)
  if (!is.na(negative_border_tight) && negative_border_tight < negative_border) {
    negative_border <- negative_border_tight
    negative_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var <= negative_border,
                                      var = as.name(variable_var)))    
    positive_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var > negative_border,
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
    abline(h = filled_border, col = "black")
    abline(v = dens_smooth$x[minima_idx])
    abline(v = dens_smooth$x[maxima_idx], col = "grey")
    abline(v = negative_border_tight, col = "red")
  }
  
  # detemine if the well has a statistically significant negative cluster 
  signif_negative_cluster <-
    has_signif_negative_cluster(plate, nrow(negative_drops), nrow(positive_drops))
  
  return(list(
    'negative_border'  = as.integer(negative_border),
    'filled_border'    = filled_border,
    'significant_negative_cluster' = signif_negative_cluster
  ))
}

#' Mark the clusters of droplets only in certain wells to their assigned cluster
#' 
#' This function simply looks at all droplets of certain wells and marks the
#' cluster of each droplet according to the gates that are already calculated.
#' This function is called once after \code{\link[ddpcr]{classify_droplets_single}}
#' determines the gates for every well, and it's called again when reclassifying
#' wells gives us more accurate information.
#' @export
#' @keywords internal
mark_clusters <- function(plate, wells) {
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)
  
  CLUSTER_RAIN <- plate %>% cluster('RAIN')
  CLUSTER_POSITIVE <- plate %>% cluster('POSITIVE')
  CLUSTER_NEGATIVE <- plate %>% cluster('NEGATIVE')
  CLUSTERS_UNANALYZED <- unanalyzed_clusters(plate, 'RAIN')
  
  # for every well in the list of wells we're interested in classifying,
  # get the filled border and the negative border, and based on that classify
  # all droplets as rain, positive, or negative
  data <-
    plate_data(plate) %>%
    dplyr::group_by_("well") %>%
    dplyr::do({
      well_data <- .
      well_id = well_data[['well']][1]
      if (!well_id %in% wells) {
        return(well_data)
      }
      
      filled_border <- well_info(plate, well_id, 'filled_border')
      negative_border <- well_info(plate, well_id, meta_var_name(plate, 'negative_border'))
      
      classifiable_idx <- well_data[['cluster']] %in% CLUSTERS_UNANALYZED
      filled_idx <- classifiable_idx & well_data[[positive_var]] >= filled_border
      negative_idx <- filled_idx & well_data[[variable_var]] <= negative_border
      positive_idx <- filled_idx & well_data[[variable_var]] > negative_border
      
      well_data[classifiable_idx, 'cluster'] <- CLUSTER_RAIN
      well_data[negative_idx, 'cluster'] <- CLUSTER_NEGATIVE
      well_data[positive_idx, 'cluster'] <- CLUSTER_POSITIVE
      well_data
    }) %>%
    dplyr::ungroup()
  
  plate_data(plate) <- data
  plate
}

#' Does a well have a statistically significant number of negative droplets?
#' 
#' Classify a well as having a significant negative cluster (eg. a mutant well)
#' or not using a binomial test.\cr\cr
#' We can call a well as mutant if it is statistically significantly more than
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
#' @param plate A ddPCR plate
#' @param neg Number of negative (or mutant) drops
#' @param pos Number of positive (or wildtype) drops
#' @return \code{TRUE} if the number of negative drops is statistically
#' significant, \code{FALSE} otherwise.
#' @export
#' @keywords internal
has_signif_negative_cluster <- function(plate, neg, pos) {
  freq_threshold <- params(plate, 'CLASSIFY', 'SIGNIFICANT_NEGATIVE_FREQ')
  pval <- params(plate, 'CLASSIFY', 'SIGNIFICANT_P_VALUE')
  total <- neg + pos
  result <- 
    (1 - pbinom(neg, total, freq_threshold) + dbinom(neg, total, freq_threshold)) < pval
  result
}