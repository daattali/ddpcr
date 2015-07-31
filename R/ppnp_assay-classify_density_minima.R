## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# Advantage of density over normal: faster, better at finding mutant drops in
# wells with low mutant frequency, works with the case where there are tons of
# "HEX rain" naturally (in normal approach, I first tried fitting two normals,
# if they're too close then fit three and assume that the middle one was capturing
# the rain).
# Disadvantage: we're not directly modeling just 2 peaks, we allow for any number.
# This method does not naturally make a wildtype/mutant call (maybe that's a good thing?)
classify_droplets_density_minima <- function(plate, well_id, plot = FALSE) {

  stopifnot(plate %>% inherits("ppnp_assay"))
  
  signif_negative_cluster <- FALSE
  msg <- NA  
  well_data <- get_single_well(plate, well_id)
  
  positive_var <- positive_dim_var(plate)
  variable_var <- variable_dim_var(plate)
  
  filled_borders <- get_filled_borders(plate, well_id)
  filled <- get_filled_drops(plate, well_id, filled_borders)
  
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
    #abline(v = negative_borders, col = "purple3")
    #abline(v = positive_borders, col = "green3")
  }
  
  # TODO come up with a better statistical solution here
  signif_negative_cluster <- (negative_freq > 5 | nrow(negative_drops) > 30)
  
  return(list(
    'negative_borders' = negative_borders %>% border_to_str,
    'positive_borders' = positive_borders %>% border_to_str,
    'filled_borders'   = filled_borders %>% border_to_str,
    'significant_negative_cluster' = signif_negative_cluster,
    'comment'          = msg
  ))
}