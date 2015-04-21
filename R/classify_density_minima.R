analyze_well_clusters_density_minima <- function(plate, well_id, plot = FALSE) {

  has_mt_cluster <- FALSE
  msg <- NA  
  well_data <- get_single_well(plate, well_id)
  
  cl_borders <- get_filled_borders(plate, well_id)
  top <-
    well_data %>%
    dplyr::filter_(~ FAM %btwn% cl_borders)
  
  # whether or not the result is found using the first attempted bandwidth
  bw_orig <- TRUE
  # previous "adjust" value
  adj_prev <- NULL 
  # final "adjust" value
  adj_final <- NULL 
  for (adj in seq(params(plate, 'ASSIGN_CLUSTERS', 'ADJUST_MIN'),
                  params(plate, 'ASSIGN_CLUSTERS', 'ADJUST_MAX'),
                  0.5)) {
    dens_smooth <- density(top[['HEX']], bw = "sj", adjust = adj)
    maxima_idx <- local_maxima(dens_smooth$y)
    minima_idx <- local_minima(dens_smooth$y)
    
    # ensure the right peak is centered on the wildype cluster and not a couple
    # outlier drops
    while (TRUE) {
      btwn_right_mins <-
        top %>%
        dplyr::filter_(~ HEX %btwn% dens_smooth$x[tail(minima_idx, 2)])
      
      if (nrow(btwn_right_mins) < nrow(top) * 0.1) {
        # discard rightmost inflection points
        maxima_idx %<>% head(-1)
        minima_idx %<>% head(-1)
        msg <- "Ignored small cluster of drops to the right of the wild-type cluster"
      } else {
        break
      }
    }
    
    num_peaks <- length(maxima_idx)
    if (num_peaks == 1) {
      if (!bw_orig) {
        adj <- adj_prev
        dens_smooth <- density(top[['HEX']], bw = "sj", adjust = adj)
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
  
  mt_borders <- c(0, right_border)
  wt_borders <- c(mt_borders[2] + 1, max(top[['HEX']]))
  mt_drops <- top %>% dplyr::filter_(~ HEX %btwn% mt_borders)
  wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
  
  # to try to replicate the human approach, we want to get the mutant border as
  # close as possible to the mutant cluster rather than in the middle between
  # mutants and wildtypes
  mt_tight_border <- mean(mt_drops[['HEX']]) +
    sd(mt_drops[['HEX']]) * 3
  if (!is.na(mt_tight_border) && mt_tight_border < right_border) {
    mt_borders[2] <- mt_tight_border
    wt_borders[1] <- mt_borders[2] + 1
    mt_drops <- top %>% dplyr::filter_(~ HEX %btwn% mt_borders)
    wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
  }  
  
  mt_freq <- round(nrow(mt_drops) / (nrow(mt_drops) + nrow(wt_drops)) * 100, 3)
  
  if (plot) {
    graphics::plot(
      well_data,
      main = sprintf("%s (%s%%)\nadj=%s (bw: %s)", well_id, mt_freq, adj_final, round(dens_smooth$bw)),
      xlab = paste0("max: ", paste(round(dens_smooth$x[maxima_idx]), collapse = ", "),
                    "\nmin: ", paste(round(dens_smooth$x[minima_idx]), collapse = ", ")))
    points(top, col = "blue")
    points(mt_drops, col = "purple")
    points(wt_drops, col = "green")
    abline(h = cl_borders %>% mean, col = "grey")
    abline(h = cl_borders, col = "black")
    abline(v = dens_smooth$x[minima_idx])
    abline(v = dens_smooth$x[maxima_idx], col = "grey")
    #abline(v = mt_borders, col = "purple")
    #abline(v = wt_borders, col = "green")
  }
  
  has_mt_cluster <- (mt_freq > 5 | nrow(mt_drops) > 30)
  
  return(list(#result = well_data,
    mt_borders = mt_borders %>% border_to_str,
    wt_borders = wt_borders %>% border_to_str,
    cl_borders = cl_borders %>% border_to_str,
    has_mt_cluster = has_mt_cluster,
    comment = msg))
}