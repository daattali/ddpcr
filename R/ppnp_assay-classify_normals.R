classify_droplets_normal <- function(plate, well_id, plot = FALSE) {
  # Given a well with the empty drops marked as empty, analyze the rest of the
  # drops and assign each to a cluster (rain/mutant/wildtype)
  #
  # Args:
  #   wellNum: The id of the well of interest
  #   plot: If true, plot the result (used mainly for development/debugging)
  #
  # Returns:
  #   list:
  #     result: Dataframe with the non-empty drops of a given well assigned to clusters
  #     hasMTcluster: TRUE if a significant mutant cluster was found, FALSE otherwise.
  #       Note that TRUE can be very good proxy for saying the well has mutant BRAFV600,
  #       and FALSE is a proxy for saying the well has wild-type BRAFV600
  #     comment: any comment raised by the algorithm, or NA if everything ran smoothly
  #
  # Algorithm:
  #   
  
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
  
  for (i in seq(params(plate, 'ASSIGN_CLUSTERS', 'NUM_ATTEMPTS_SEGREGATE'))) {
    quiet(
      mixmdl <- mixtools::normalmixEM(filled[[variable_var]], k = 2))
    segregate_ratio <- params(plate, 'ASSIGN_CLUSTERS', 'SEGREGATE_RATIO_THRESHOLD')
    if (min(mixmdl$mu) < max(mixmdl$mu) * segregate_ratio) {
      signif_negative_cluster <- TRUE
      break
    }
  }
  
  smaller_comp <- mixmdl$mu %>% which.min
  larger_comp <- mixmdl$mu %>% which.max
  negative_borders <-
    plus_minus(
      mixmdl$mu[smaller_comp],
      mixmdl$sigma[smaller_comp] *
        params(plate, 'ASSIGN_CLUSTERS', 'CLUSTERS_BORDERS_NUM_SD')
    ) %>%
    as.integer
  positive_borders <-
    plus_minus(
      mixmdl$mu[larger_comp],
      mixmdl$sigma[larger_comp] *
        params(plate, 'ASSIGN_CLUSTERS', 'CLUSTERS_BORDERS_NUM_SD')
    ) %>%
    as.integer
  
  set.seed(SEED)
  if (signif_negative_cluster) {
    # if the rain in the HEX is too strong, it's possible to either have overlapping
    # borders or have the mutant cluster being wider than the wildtype cluster
    # (which shouldn't happen), so in both cases, we just fit three gaussians
    # instead of 2, and use the right and left ones are the wild-type/mutant clusters
    use_three_clusters <- FALSE
    if (positive_borders[1] < negative_borders[2]) {
      use_three_clusters <- TRUE
      msg <- sprintf("using 3 %s gaussians to find clusters (%s and %s cluster borders overlapped)",
                     variable_var, params(plate, 'GENERAL', 'NEGATIVE_NAME'),
                     params(plate, 'GENERAL', 'POSITIVE_NAME'))
    } else if (mixmdl$sigma[smaller_comp] > mixmdl$sigma[larger_comp]) {
      use_three_clusters <- TRUE
      msg <- sprintf("using 3 %s gaussians to find clusters (SD of %s cluster > SD of %s cluster)",
                     variable_var, params(plate, 'GENERAL', 'NEGATIVE_NAME'),
                     params(plate, 'GENERAL', 'POSITIVE_NAME'))
    }
    
    if (use_three_clusters) {
      quiet(
        mixmdl <- mixtools::normalmixEM(filled[[variable_var]], k = 3))
      smaller_comp <- mixmdl$mu %>% which.min
      larger_comp <- mixmdl$mu %>% which.max
      negative_borders <-
        plus_minus(
          mixmdl$mu[smaller_comp],
          mixmdl$sigma[smaller_comp] *
            params(plate, 'ASSIGN_CLUSTERS', 'CLUSTERS_BORDERS_NUM_SD')
        ) %>% 
        as.integer
      positive_borders <-
        plus_minus(
          mixmdl$mu[larger_comp],
          mixmdl$sigma[larger_comp] *
            params(plate, 'ASSIGN_CLUSTERS', 'CLUSTERS_BORDERS_NUM_SD')
        ) %>%
        as.integer
    }
  } else {
    negative_cutoff <-
      mixmdl$mu[larger_comp] -
      mixmdl$sigma[larger_comp] * 
      params(plate, 'ASSIGN_CLUSTERS', 'NO_NEG_CLUSTER_BORDER_NUM_SD')
    negative_borders <- c(0, negative_cutoff) %>% as.integer
  }
  
  if (plot) {
    negative_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var %btwn% negative_borders,
                                      var = as.name(variable_var)))      
    positive_drops <- filled %>%
      dplyr::filter_(lazyeval::interp(~ var %btwn% positive_borders,
                                      var = as.name(variable_var)))
    graphics::plot(well_data, variable_var, positive_var)
    points(filled, col = "blue")
    points(negative_drops, col = "purple")
    points(positive_drops, col = "green")
    abline(h = filled_borders %>% mean, col = "grey")
    abline(h = filled_borders, col = "black")
    abline(v = mixmdl$mu, col = "grey")
    abline(v = negative_borders, col = "purple")
    abline(v = positive_borders, col = "green")
    negative_freq <- calc_negative_freq_simple(nrow(negative_drops), nrow(positive_drops))
    title(sprintf("%s (%s%%)", well_id, negative_freq))
  }
  
  return(list(
    'negative_borders' = negative_borders %>% border_to_str,
    'positive_borders' = positive_borders %>% border_to_str,
    'filled_borders'   = filled_borders %>% border_to_str,
    'significant_negative_cluster' = signif_negative_cluster,
    'comment'          = msg
  ))
}