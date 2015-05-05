define_clusters.ppnp_assay <- function(plate) {
  clusters <- NextMethod("define_clusters")
  
  clusters %>%
    add_clusters(c(
      'RAIN',
      'POSITIVE',
      'NEGATIVE'
    ))
}

define_steps.ppnp_assay <- function(plate) {
  steps <- NextMethod("define_steps")
  
  steps %>%
    add_steps(list(
      'CLASSIFY' = 'classify_droplets',
      'RECLASSIFY' = 'reclassify_droplets'
    ))
}

define_params.ppnp_assay <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME'      = 'positive',
      'NEGATIVE_NAME'      = 'negative',
      'POSITIVE_DIMENSION' = NA   # Must be set by the child
    ),
    'REMOVE_FAILURES' = list(
      'FAST'   = TRUE
    ),
    'CLASSIFY' = list(
      'NUM_ATTEMPTS_SEGREGATE'       = 1,
      'SEGREGATE_RATIO_THRESHOLD'    = 0.75,
      'CLUSTERS_BORDERS_NUM_SD'      = 3,
      'NO_NEG_CLUSTER_BORDER_NUM_SD' = 10,
      'ADJUST_MIN'                   = 4,
      'ADJUST_MAX'                   = 20,
      'METHOD'                       = 'density_inflection_points'
    ),
    'RECLASSIFY' = list(
      'MIN_WELLS_NEGATIVE_CLUSTER'   = 4,
      'BORDER_RATIO_QUANTILE'        = 0.75
    )
  )
  params %<>% modifyList(new_params)
  
  params
}

positive_dim <- function(plate) {
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') 
}

variable_dim <- function(plate) {
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% other_dim
}

# get the name of the variable that is along the dimension where all filled 
# filled should be positive
positive_dim_var <- function(plate) {
  plate %>%
    positive_dim %>%
    toupper %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}
}

# get the name of the variable that is along the dimension where the droplets
# will cluster into two groups
variable_dim_var <- function(plate) {
  plate %>%
    variable_dim %>%
    toupper %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}  
}

# given an axis (X or Y), return the other
other_dim <- function(dim) {
  ifelse(dim == "X", "Y", "X")
}

meta_var_name <- function(plate, var) {
  var %>%
    gsub("negative", params(plate, 'GENERAL', 'NEGATIVE_NAME'), .) %>%
    gsub("positive", params(plate, 'GENERAL', 'POSITIVE_NAME'), .)
}

calc_negative_freq_simple <- function(negative_drops, positive_drops) {
  (negative_drops / (negative_drops + positive_drops)) %>%
    magrittr::multiply_by(100) %>%
    signif(3)
}

calculate_neg_freq_single <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  
  negative_num <- (well_data[['cluster']] == plate %>% cluster('NEGATIVE')) %>% sum
  positive_num <- (well_data[['cluster']] == plate %>% cluster('POSITIVE')) %>% sum
  negative_freq <- calc_negative_freq_simple(negative_num, positive_num)
  
  list(negative_num = negative_num,
       positive_num = positive_num,
       negative_freq = negative_freq)
}

calculate_negative_freqs <- function(plate) {
  negative_freqs <-
    vapply(wells_success(plate),
           function(x) calculate_neg_freq_single(plate, x),
           vector(mode = "list", length = 3)) %>%
    lol_to_df %>%
    magrittr::set_names(lapply(names(.), function(x) meta_var_name(plate, x)))
  
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(negative_freqs)
  
  plate
}

get_filled_borders <- function(plate, well_id) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  
  well_data <- get_single_well(plate, well_id)
  
  set.seed(SEED)
  
  positive_var <- positive_dim_var(plate)
  quiet(
    mixmdl_pos <- mixtools::normalmixEM(well_data[[positive_var]], k = 2))
  larger_comp_pos <- mixmdl_pos$mu %>% which.max
  filled_borders <-
    plus_minus(
      mixmdl_pos$mu[larger_comp_pos],
      mixmdl_pos$sigma[larger_comp_pos] *
        params(plate, 'CLASSIFY', 'CLUSTERS_BORDERS_NUM_SD')
    ) %>%
    as.integer
  
  filled_borders
}

#' @export
wells_positive <- function(x) {
  stopifnot(x %>% inherits("ppnp_assay"))
  
  if (status(x) < step(x, 'CLASSIFY')) {
    return(c())
  }
  
  x %>%
    plate_meta %>%
    dplyr::filter_(lazyeval::interp(
      ~ !var,
      var = as.name(meta_var_name(x, "significant_negative_cluster"))
    )) %>%
    .[['well']]
}

#' @export
wells_negative <- function(x) {
  stopifnot(x %>% inherits("ppnp_assay"))
  
  if (status(x) < step(x, 'CLASSIFY')) {
    return(c())
  }  
  
  x %>%
    plate_meta %>%
    dplyr::filter_(as.name(meta_var_name(x, "significant_negative_cluster"))) %>%
    .[['well']]
}

is_well_success.ppnp_assay <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(list(success = NA, comment = NA))
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params(plate, 'REMOVE_FAILURES', 'TOTAL_DROPS_T')) {
    success <- FALSE
    msg <- sprintf("Not enough drops generated (%s)", nrow(well_data))
    return(list(success = success, comment = msg))
  }
  
  set.seed(SEED)
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  if (params(plate, 'REMOVE_FAILURES', 'FAST')) {
    kmeans_y <- kmeans(well_data[[y_var]], 2, nstart = 5)
    centers_y <- kmeans_y$centers %>% as.integer
    smaller_comp_y <- centers_y %>% which.min
    
    if ((centers_y %>% diff %>% abs) < min(centers_y)) {
      success <- FALSE
      msg <- sprintf("There seems to be mostly empty drops (centers of %s clusters: %s)",
                     y_var, paste0(centers_y, collapse = ","))
      return(list(success = success, comment = msg))
    }
    
    smaller_lambda <- kmeans_y$size[smaller_comp_y]/sum(kmeans_y$size)
    
    if (smaller_lambda < params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_LOW_T')) {
      success <- FALSE
      msg <- paste0("Could not find significant empty cluster (lambda of ", y_var, " normal: ",
                    signif(smaller_lambda, 4), ")")
      return(list(success = success, comment = msg))
    }
    
    if (smaller_lambda > params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_HIGH_T')) {
      success <- FALSE
      msg <- paste0("There are too many empty drops (lambda of ", y_var, " normal: ",
                    signif(smaller_lambda, 4), ")")
      return(list(success = success, comment = msg))
    }
    
    return(list(success = TRUE, comment = NA)) 
  }
  
  # fit two normal distributions in the data along the Y dimension, check:
  # - the mu's of the two populations needs to be far enough
  # - bottom population needs to have lambda not too small and not too large
  # - sigma of bottom population should be fairly small  
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[y_var]], k = 2))
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  larger_comp_y <- mixmdl_y$mu %>% which.max
  
  if ((mixmdl_y$mu %>% diff %>% abs) < min(mixmdl_y$mu)) {
    success <- FALSE
    msg <- paste0("There seems to be mostly empty drops (mu's of ", y_var, " normals: ",
                  paste0(round(mixmdl_y$mu), collapse = " "), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] < params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_LOW_T')) {
    success <- FALSE
    msg <- paste0("Could not find significant empty cluster (lambda of ", y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] > params(plate, 'REMOVE_FAILURES', 'NORMAL_LAMBDA_HIGH_T')) {
    success <- FALSE
    msg <- paste0("There are too many empty drops (lambda of ", y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$sigma[smaller_comp_y] > params(plate, 'REMOVE_FAILURES', 'NORMAL_SIGMA_T')) {
    success <- FALSE
    msg <- paste0("Could not find a dense empty cluster (sigma of ", y_var, " normal: ",
                  round(mixmdl_y$sigma[smaller_comp_y]), ")")
    return(list(success = success, comment = msg))
  }    
  
  # if all the sanity checks passed, the run was successful
  return(list(success = TRUE, comment = NA))    
}




#' @export
plot.ppnp_assay <- function(
  x,
  wells, samples,
  col_drops_negative = "purple", col_drops_positive = "darkgreen",
  col_drops_rain = "black",
  show_negative_freq = TRUE, text_size_negative_freq = 4,
  alpha_drops_low_negative_freq = 0.5,
  show_low_high_neg_freq = TRUE,
  bg_negative = "purple", bg_positive = "darkgreen",
  alpha_bg_low_high_neg_freq = 0.1,
  superimpose = FALSE, show_drops = TRUE,
  ...)
{
  p <- NextMethod("plot",
                  show_drops = show_drops, superimpose = superimpose)
  
  plate <- subset(x, wells, samples)
  rm(x)
  
  # prepare the data to be plotted
  meta <- plate_meta(plate)
  meta[['row']] %<>% as.factor
  meta[['col']] %<>% as.factor  
  meta_used <- meta %>% dplyr::filter_(~ used)
  
  if (status(plate) < step(plate, 'CLASSIFY')) {
    show_negative_freq = FALSE
    show_low_high_neg_freq = FALSE
  }
  
  # superimpose all the data from all the wells onto one plot instead of a grid
  if (superimpose) {
    show_drops = TRUE
    show_negative_freq = FALSE
    show_low_high_neg_freq = FALSE
  }
  
  # define the colours of the backgrounds of wells with high/low MT freq
  if (wells_positive(plate) %>% length > 0) {
    bg_cols <- c(bg_positive, bg_negative)
  } else {
    bg_cols <- c(bg_negative)
  }
  
  # extract the mutant drops in wildtype wells
  neg_drops_low_freq <-
    plate %>%
    plate_data %>%
    dplyr::filter_(~ well %in% wells_positive(plate)) %>%
    dplyr::filter_(~ cluster == cluster(plate, 'NEGATIVE'))
  neg_drops_low_freq[['row']] <- substring(neg_drops_low_freq[['well']], 1, 1) %>% as.factor
  neg_drops_low_freq[['col']] <- substring(neg_drops_low_freq[['well']], 2, 3) %>% as.integer %>% as.factor
  
  # show the drops
  if (show_drops) {
    if (neg_drops_low_freq %>% nrow > 0) {
      p <- p +
        ggplot2::geom_point(
          data = neg_drops_low_freq,
          ggplot2::aes_string(x_var(plate), y_var(plate)),
          alpha = alpha_drops_low_negative_freq,
          col = col_drops_negative)
    }
  }
  
  # show mutant vs wildtype well as background colour
  if (show_low_high_neg_freq) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = meta_used %>% dplyr::filter_(~ success),
          ggplot2::aes_string(fill = meta_var_name(plate, "significant_negative_cluster"),
                              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = alpha_bg_low_high_neg_freq,
          show_guide = FALSE) +
        ggplot2::scale_fill_manual(values = bg_cols)
    }
  }
  
  ggb <- ggplot2::ggplot_build(p)

  # show the mutant frequency as text on the plot
  if (show_negative_freq) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {
      
      if (!show_drops) {
        text_pos_x <- 0
        text_pos_y <- 0
        hjust = 0.5
        vjust = 0.5
      } else if (positive_dim(plate) == "Y") {
        text_pos_x <- ggb$panel$ranges[[1]]$x.range[1] +
          diff(ggb$panel$ranges[[1]]$x.range) * 0.95
        text_pos_y <- ggb$panel$ranges[[1]]$y.range[1] +
          diff(ggb$panel$ranges[[1]]$y.range) * 0.1
        hjust = 1
        vjust = 0
      } else {
        text_pos_x <- ggb$panel$ranges[[1]]$x.range[1] +
          diff(ggb$panel$ranges[[1]]$x.range) * 0.1
        text_pos_y <- ggb$panel$ranges[[1]]$y.range[1] +
          diff(ggb$panel$ranges[[1]]$y.range) * 0.95
        hjust = 0
        vjust = 1
      }
    
      meta_used[['neg_freq_text']] <-
        paste0(meta_used[[meta_var_name(plate, 'negative_freq')]], "%")
      p <- p + 
        ggplot2::geom_text(
          data = dplyr::filter_(meta_used, ~ success),
          ggplot2::aes_string(label = "neg_freq_text"),
          x = text_pos_x, y = text_pos_y,
          hjust = hjust, vjust = vjust,
          fontface = "bold",
          size = text_size_negative_freq,
          show_guide = FALSE)
    }
  }
  
  p
}