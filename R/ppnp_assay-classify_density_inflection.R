classify_droplets_density_inflection_points <- function(plate, well_id, plot = FALSE) {
  stopifnot(plate %>% inherits("ppnp_assay"))

  signif_negative_cluster <- FALSE
  msg <- NA  
  well_data <- get_single_well(plate, well_id)
  
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)

  filled_borders <- get_filled_borders(plate, well_id)
  filled <-
    well_data %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% filled_borders,
                                    var = as.name(positive_var)))
  
  # whether or not the result is found using the first attempted bandwidth
  bw_orig <- TRUE
  # previous "adjust" value
  adj_prev <- NULL 
  # final "adjust" value
  adj_final <- NULL 
  for (adj in seq(params(plate, 'CLASSIFY', 'ADJUST_MIN'),
                  params(plate, 'CLASSIFY', 'ADJUST_MAX'),
                  0.5)) {
    dens_smooth <- density(filled[[variable_var]], bw = "sj", adjust = adj)
    inflection_idx <- get_inflection_pts(dens_smooth)
    maxima_idx <- local_maxima(dens_smooth$y)
    
    # ensure the right peak/inflection points are covering the wildtype drops
    # and not a couple outliers
    while (TRUE) {
      positive_borders <- dens_smooth$x[tail(inflection_idx, 2)]
      positive_drops <- filled %>%
        dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                        var = as.name(variable_var)))
      if (nrow(positive_drops) < nrow(filled) * 0.1) {
        # discard rightmost inflection points
        inflection_idx %<>% head(-2)
        if (dens_smooth$x[tail(maxima_idx, 1)] %btwn% positive_borders) {
          maxima_idx %<>% head(-1)
        }
        msg <- sprintf("ignored small cluster of drops above the %s cluster",
                       params(plate, 'GENERAL', 'POSITIVE_NAME'))
      } else {
        break
      }
    }
    
    num_peaks <- length(maxima_idx)
    num_inf <- length(inflection_idx)
    if (num_peaks == 1) {
      if (!bw_orig) {
        adj <- adj_prev
        dens_smooth <- density(filled[[variable_var]], bw = "sj", adjust = adj)
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
    warn_msg(sprintf("Could not analyze well %s", well_id))
    return()
  }
  
  if (num_peaks == 1) {
    negative_borders <- c(0, 0)
  } else {
    # use the IP to the right of the left-most peak
    left_peak <- dens_smooth$x[maxima_idx][1]
    inf_pts <- dens_smooth$x[inflection_idx]
    right_border <- inf_pts[which(inf_pts > left_peak) %>% min]
    negative_borders <- c(0, right_border)
  }
  
  positive_borders <- dens_smooth$x[tail(inflection_idx, 2)]
  negative_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% negative_borders,
                                    var = as.name(variable_var)))
  positive_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                    var = as.name(variable_var)))
  
  # TODO this is fairly hack-y: if the mutant frequency is high enough,
  # we manually move the border a bit to make sure we aren't too stringent
  # and losing mutant drops
  negative_freq <- calc_negative_freq_simple(nrow(negative_drops), nrow(positive_drops))

  if (negative_freq > 1) {
    negative_borders[2] <- negative_borders[2] +
                          (positive_borders[1] - negative_borders[2]) * 0.1
  }
  
  negative_borders[1] <- 0
  positive_borders[2] <- max(filled[[variable_var]])
  positive_borders[1] <- negative_borders[2] + 1
  negative_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% negative_borders,
                                    var = as.name(variable_var)))    
  positive_drops <- filled %>%
    dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                    var = as.name(variable_var)))        
  negative_freq <- calc_negative_freq_simple(nrow(negative_drops), nrow(positive_drops))
  
  if (plot) {
    graphics::plot(
      well_data,
      main = sprintf("%s (%s%%)\nadj=%s (bw: %s)", well_id, negative_freq,
                     adj_final, round(dens_smooth$bw)),
      xlab = paste0("max: ", paste(round(dens_smooth$x[maxima_idx]), collapse = ", "),
                    "\ninf: ", paste(round(dens_smooth$x[inflection_idx]), collapse = ", ")))
    points(filled, col = "blue")
    points(negative_drops, col = "purple")
    points(positive_drops, col = "green")
    abline(h = filled_borders %>% mean, col = "grey")
    abline(h = filled_borders, col = "black")
    abline(v = dens_smooth$x[inflection_idx])
    abline(v = dens_smooth$x[maxima_idx], col = "grey")
    #abline(v = negative_borders, col = "purple")
    #abline(v = positive_borders, col = "green")
  }
  
  # TODO better way to decide if there is a significant mutant cluster
  signif_negative_cluster <- (negative_freq > 5 | nrow(negative_drops) > 30)
  
  return(list(
    'negative_borders' = negative_borders %>% border_to_str,
    'positive_borders' = positive_borders %>% border_to_str,
    'filled_borders'   = filled_borders %>% border_to_str,
    'significant_negative_cluster' = signif_negative_cluster,
    'comment'          = msg
  ))
}