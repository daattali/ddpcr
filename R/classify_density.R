analyze_well_clusters_density <- function(plate, well_id, plot = FALSE) {
  
  params <- params(plate)

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
  for (adj in seq(params[['ASSIGN_CLUSTERS']][['ADJUST_MIN']],
                  params[['ASSIGN_CLUSTERS']][['ADJUST_MAX']],
                  0.5)) {
    dens_smooth <- density(top[['HEX']], bw = "sj", adjust = adj)
    inflection_idx <- get_inflection_pts(dens_smooth)
    maxima_idx <- local_maxima(dens_smooth$y)
    
    # ensure the right peak/inflection points are covering the wildtype drops
    # and not a couple outliers
    while (TRUE) {
      wt_borders <- dens_smooth$x[tail(inflection_idx, 2)]
      wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
      if (nrow(wt_drops) < nrow(top) * 0.1) {
        # discard rightmost inflection points
        inflection_idx %<>% head(-2)
        if (dens_smooth$x[tail(maxima_idx, 1)] %btwn% wt_borders) {
          maxima_idx %<>% head(-1)
        }
        msg <- "Ignored small cluster of drops to the right of the wild-type cluster"
      } else {
        break
      }
    }
    
    num_peaks <- length(maxima_idx)
    num_inf <- length(inflection_idx)
    if (num_peaks == 1) {
      if (!bw_orig) {
        adj <- adj_prev
        dens_smooth <- density(top[['HEX']], bw = "sj", adjust = adj)
        inflection_idx <- get_inflection_pts(dens_smooth)
        maxima_idx <- local_maxima(dens_smooth$y)
        num_peaks <- length(maxima_idx)
        num_inf <- length(inflection_idx)
      }
      adj_final <- adj
      break
    }
    if (num_peaks == 2 && num_inf == 4) {
      adj_final <- adj
      break
    }
    adj_prev <- adj
    bw_orig <- FALSE
  }
  if (is.null(adj_final)) {
    err_msg(sprintf("Could not analyze well %s", well_id))
  }
  
  if (num_peaks == 1) {
    mt_borders <- c(0, 0)
  } else {
    # use the IP to the right of the left-most peak
    left_peak <- dens_smooth$x[maxima_idx][1]
    inf_pts <- dens_smooth$x[inflection_idx]
    right_border <- inf_pts[which(inf_pts > left_peak) %>% min]
    mt_borders <- c(0, right_border)
  }
  
  wt_borders <- dens_smooth$x[tail(inflection_idx, 2)]
  mt_drops <- top %>% dplyr::filter_(~ HEX %btwn% mt_borders)
  wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
  mt_freq <- round(nrow(mt_drops) / (nrow(mt_drops) + nrow(wt_drops)) * 100, 3)
  
  if (mt_freq > 1) {
    mt_borders[2] <- mt_borders[2] + (wt_borders[1] - mt_borders[2]) * 0.1
  }
  
  mt_borders[1] <- 0
  wt_borders[2] <- max(top[['HEX']])
  wt_borders[1] <- mt_borders[2] + 1
  mt_drops <- top %>% dplyr::filter_(~ HEX %btwn% mt_borders)
  wt_drops <- top %>% dplyr::filter_(~ HEX %btwn% wt_borders)
  mt_freq <- round(nrow(mt_drops) / (nrow(mt_drops) + nrow(wt_drops)) * 100, 3)
  
  if (plot) {
    graphics::plot(
      well_data,
      main = sprintf("%s (%s%%)\nadj=%s (bw: %s)", well_id, mt_freq, adj_final, round(dens_smooth$bw)),
      xlab = paste0("max: ", paste(round(dens_smooth$x[maxima_idx]), collapse = ", "),
                    "\ninf: ", paste(round(dens_smooth$x[inflection_idx]), collapse = ", ")))
    points(top, col = "blue")
    points(mt_drops, col = "purple")
    points(wt_drops, col = "green")
    abline(h = cl_borders %>% mean, col = "grey")
    abline(h = cl_borders, col = "black")
    abline(v = dens_smooth$x[inflection_idx])
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