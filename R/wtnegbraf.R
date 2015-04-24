STATUS_DROPLETS_CLASSIFIED     <- (STATUS_EMPTY_REMOVED + 1) %>% as.integer
STATUS_DROPLETS_RECLASSIFIED   <- (STATUS_EMPTY_REMOVED + 2) %>% as.integer

CLUSTER_WT         <- (CLUSTER_EMPTY + 1) %>% as.integer
CLUSTER_MT         <- (CLUSTER_EMPTY + 2) %>% as.integer
CLUSTER_RAIN       <- (CLUSTER_EMPTY + 3) %>% as.integer

parent_plate_type.wtnegbraf <- function(plate) {
  "ppnp_assay"
}

default_params.wtnegbraf <- function(plate) {
  params <- NextMethod("default_params")
  
  PARAMS_ASSIGN_CLUSTERS <- list()
  PARAMS_ASSIGN_CLUSTERS['NUM_ATTEMPTS_SEGREGATE'] <- 1
  PARAMS_ASSIGN_CLUSTERS['SEGREGATE_RATIO_THRESHOLD'] <- 0.75
  PARAMS_ASSIGN_CLUSTERS['CLUSTERS_BORDERS_NUM_SD'] <- 3
  PARAMS_ASSIGN_CLUSTERS['NO_CLUSTER_MT_BORDER_NUM_SD'] <- 10
  PARAMS_ASSIGN_CLUSTERS['ADJUST_MIN'] <- 4
  PARAMS_ASSIGN_CLUSTERS['ADJUST_MAX'] <- 20
  PARAMS_RECLASSIFY_LOW_MT <- list()
  PARAMS_RECLASSIFY_LOW_MT['MIN_WELLS_MT_CLUSTER'] <- 4
  PARAMS_RECLASSIFY_LOW_MT['BORDER_RATIO_QUANTILE'] <- 0.75  
  
  params[['GENERAL']][['X_VAR']] <- "HEXXX"
  params[['GENERAL']][['Y_VAR']] <- "FAMMM"
  params[['WELLSUCCESS']][['FAST']] <- TRUE
  params[['ASSIGN_CLUSTERS']] <- PARAMS_ASSIGN_CLUSTERS
  params[['RECLASSIFY_LOW_MT']] <- PARAMS_RECLASSIFY_LOW_MT 
  
  params
}

is_well_success.wtnegbraf <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)
  
  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(list(success = NA, comment = NA))
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params(plate, 'WELLSUCCESS', 'TOTAL_DROPS_T')) {
    success <- FALSE
    msg <- sprintf("Not enough drops generated (%s)", nrow(well_data))
    return(list(success = success, comment = msg))
  }
  
  set.seed(SEED)
  
  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR')
  
  if (params(plate, 'WELLSUCCESS', 'FAST')) {
    kmeans_y <- kmeans(well_data[[Y_var]], 2, nstart = 5)
    centers_y <- kmeans_y$centers %>% as.integer
    smaller_comp_y <- centers_y %>% which.min
    
    if ((centers_y %>% diff %>% abs) < min(centers_y)) {
      success <- FALSE
      msg <- sprintf("There seems to be mostly empty drops (centers of %s clusters: %s)",
                     Y_var, paste0(centers_y, collapse = ","))
      return(list(success = success, comment = msg))
    }
    
    smaller_lambda <- kmeans_y$size[smaller_comp_y]/sum(kmeans_y$size)
    
    if (smaller_lambda < params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_LOW_T')) {
      success <- FALSE
      msg <- paste0("Could not find significant empty cluster (lambda of ", Y_var, " normal: ",
                    signif(smaller_lambda, 4), ")")
      return(list(success = success, comment = msg))
    }
    
    if (smaller_lambda > params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_HIGH_T')) {
      success <- FALSE
      msg <- paste0("There are too many empty drops (lambda of ", Y_var, " normal: ",
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
    mixmdl_y <- mixtools::normalmixEM(well_data[[Y_var]], k = 2))
  smaller_comp_y <- mixmdl_y$mu %>% which.min
  larger_comp_y <- mixmdl_y$mu %>% which.max
  
  if ((mixmdl_y$mu %>% diff %>% abs) < min(mixmdl_y$mu)) {
    success <- FALSE
    msg <- paste0("There seems to be mostly empty drops (mu's of ", Y_var, " normals: ",
                  paste0(round(mixmdl_y$mu), collapse = " "), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] < params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_LOW_T')) {
    success <- FALSE
    msg <- paste0("Could not find significant empty cluster (lambda of ", Y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$lambda[smaller_comp_y] > params(plate, 'WELLSUCCESS', 'NORMAL_LAMBDA_HIGH_T')) {
    success <- FALSE
    msg <- paste0("There are too many empty drops (lambda of ", Y_var, " normal: ",
                  signif(mixmdl_y$lambda[smaller_comp_y], 4), ")")
    return(list(success = success, comment = msg))
  }
  
  if (mixmdl_y$sigma[smaller_comp_y] > params(plate, 'WELLSUCCESS', 'NORMAL_SIGMA_T')) {
    success <- FALSE
    msg <- paste0("Could not find a dense empty cluster (sigma of ", Y_var, " normal: ",
                  round(mixmdl_y$sigma[smaller_comp_y]), ")")
    return(list(success = success, comment = msg))
  }    
  
  # if all the sanity checks passed, the run was successful
  return(list(success = TRUE, comment = NA))    
}

#' @export
wells_mutant <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ has_mt_cluster) %>%
    .[['well']]
}

#' @export
wells_wildtype <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ !has_mt_cluster) %>%
    .[['well']]
}

#' @export
analyze.wtnegbraf = function(plate) {
  plate <- NextMethod("analyze")
  plate %<>% classify_droplets   # step 4 - classify droplets as mutant/wildtype/rain
  plate %<>% reclassify_droplets # step 5 - reanalyze low mutant frequency wells
  
  plate
}


#' @export
plot.wtnegbraf <- function(
  plate,
  wells, samples,
  superimpose = FALSE, show_full_plate = FALSE,
  show_drops = TRUE, show_empty = FALSE, show_outliers = FALSE,
  show_failed = TRUE, show_mt_vs_wt = TRUE,
  show_mt_freq = TRUE, mt_freq_size = 4,
  col_failed = "black", col_empty = "black",
  col_wt = "green", col_mt = "purple", col_rain = "black", col_outlier = "orange",
  bg_failed = "#111111", bg_unused = "#FFFFFF", bg_wt = "green", bg_mt = "purple",
  alpha_drops = 0.1, alpha_drops_low_mt = 0.5, alpha_drops_outlier = 1,
  alpha_bg_failed = 0.7, alpha_bg_mt_wt = 0.1,
  xlab = "HEX", ylab = "FAM", title,
  show_grid = FALSE, label_axes = FALSE)
{
  
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  meta <- plate_meta(plate)
  data <- plate_data(plate)
  
  if (status(plate) < STATUS_DROPLETS_CLASSIFIED) {
    show_mt_freq = FALSE
  }
  
  # prepare the data to be plotted
  rownames(meta) <- meta[['well']]
  meta[['row']] %<>% as.factor
  meta[['col']] %<>% as.factor
  data[['row']] <- substring(data[['well']], 1, 1) %>% as.factor
  data[['col']] <- substring(data[['well']], 2, 3) %>% as.integer %>% as.factor
  data[['cluster']] %<>% as.factor
  
  if (!show_empty) {
    data %<>% dplyr::filter_(~ cluster != CLUSTER_EMPTY)
  }
  if (!show_outliers) {
    data %<>% dplyr::filter_(~ cluster != CLUSTER_OUTLIER)
  }
  if (!show_failed) {
    wells_failed <- wells_failed(plate)
    if (length(wells_failed) > 0) {
      meta[wells_failed, ][['used']] <- FALSE
      data %<>% dplyr::filter_(~ !(well %in% wells_failed))
    }
  }
  
  # if the user wants to only show certain wells/samples
  if (!missing(wells)) {
    wells <- 
      meta %>%
      dplyr::filter_(~ used, ~ well %in% wells) %>%
      .[['well']]
  } else if (!missing(samples)) {
    wells <-
      meta %>%
      dplyr::filter_(used, ~ sample %in% samples) %>%
      .[['well']]
  } else {
    wells <- unique(data[['well']])
  }
  
  if (length(wells) == 0) {
    err_msg("There are no wells to show.")
  }
  
  data %<>%  dplyr::filter_(~ well %in% wells)
  if (any(!(meta[['well']] %in% wells))) {
    meta[!(meta[['well']] %in% wells), ][['used']] <- FALSE
  }
  meta_used <- meta %>% dplyr::filter_(~ used)
  
  # define the colours of the clusters
  cluster_cols <- c(col_failed, col_outlier, col_empty, col_wt, col_mt, col_rain)
  
  # need to remove colours corresponding to clusters that don't exist in the dataset
  # because otherwise the colour order will be messed up
  if (data[['cluster']] %>% unique %>% length < cluster_cols %>% length) {
    clusters_exclude <- c()
    for (i in seq_along(cluster_cols)) {
      if (dplyr::filter_(data, ~ cluster == (i - 1)) %>% nrow == 0) {
        clusters_exclude %<>% c(i)
      }
    }
    cluster_cols <- cluster_cols[-clusters_exclude]
  }
  
  # define the colours of the backgrounds of wells with high/low MT freq
  if (wells_wildtype(plate) %>% length > 0) {
    cluster_cols_bg <- c(bg_wt, bg_mt)
  } else {
    cluster_cols_bg <- c(bg_mt)
  }
  
  # extract the mutant drops in wildtype wells
  mt_drops_wt <-
    data %>%
    dplyr::filter_(~ well %in% wells_wildtype(plate)) %>%
    dplyr::filter_(~ cluster == CLUSTER_MT)
  
  # remove unused rows/columns from the plate
  if (!show_full_plate) {
    meta %<>%
      dplyr::filter_(~ col %in% (meta_used[['col']] %>% unique),
                     ~ row %in% (meta_used[['row']] %>% unique)) %>%
      dplyr::arrange_(~ row, ~ col)
  }
  
  # basic plot
  p <-
    ggplot2::ggplot() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  if (!missing(title)) {
    p <- p +
      ggplot2::ggtitle(title)
  }  
  
  # superimpose all the data from all the wells onto one plot instead of a grid
  if (superimpose) {
    show_drops = TRUE
    show_mt_freq = FALSE
    show_mt_vs_wt = FALSE
  }
  if (!superimpose) {
    p <- p +
      ggplot2::facet_grid(row ~ col)
  }
  
  # show unused wells
  if (!superimpose) {
    if (sum(!meta[['used']], na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter_(meta, ~ !used),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = bg_unused)
    }
  }
  
  # show the drops
  if (show_drops) {
    p <- p +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes_string(x = "HEX", y = "FAM", color = "cluster"),
        alpha = alpha_drops, show_guide = FALSE) +
      ggplot2::scale_color_manual(values = cluster_cols)
    
    if (mt_drops_wt %>% nrow > 0) {
      p <- p +
        ggplot2::geom_point(
          data = mt_drops_wt,
          ggplot2::aes_string("HEX", "FAM"),
          alpha = alpha_drops_low_mt,
          col = col_mt)
    }
    
    if (show_outliers) {
      p <- p +
        ggplot2::geom_point(
          data = dplyr::filter_(data, ~ cluster == CLUSTER_OUTLIER),
          ggplot2::aes_string("HEX", "FAM"),
          alpha = alpha_drops_outlier,
          col = col_outlier)
    }
  }
  
  # show mutant vs wildtype well as background colour
  if (show_mt_vs_wt) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter_(meta_used, ~ success),
          ggplot2::aes_string(fill = "has_mt_cluster",
                              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = alpha_bg_mt_wt,
          show_guide = FALSE) +
        ggplot2::scale_fill_manual(values = cluster_cols_bg)
    }
  }
  
  # show the failed ddPCR runs
  if (show_failed && !superimpose) {
    if (sum(!meta_used[['success']], na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter_(meta_used, ~ !success),
          ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = alpha_bg_failed,
          fill = bg_failed)
    }
  }
  
  # plot visual parameters
  if (!show_grid) {
    p <- p +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  }
  if (!label_axes) {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  }
  
  # so that R won't complain when we don't show the drops
  if (!show_drops) {
    p <- p +
      ggplot2::geom_text(data = meta, ggplot2::aes(0, 0, label = ""))
  }
  
  # ensure the aspect ratio is a square for each well
  ggb <- ggplot2::ggplot_build(p)
  p <- p +
    ggplot2::coord_fixed(
      ratio = diff(ggb$panel$ranges[[1]]$x.range) / diff(ggb$panel$ranges[[1]]$y.range))
  
  # show the mutant frequency as text on the plot
  if (show_mt_freq) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {
      text_pos_hex <- ggb$panel$ranges[[1]]$x.range[1] +
        diff(ggb$panel$ranges[[1]]$x.range) * 0.95
      text_pos_fam <- ggb$panel$ranges[[1]]$y.range[1] +
        diff(ggb$panel$ranges[[1]]$y.range) * 0.1
      hjust = 1
      vjust = 0
      
      # if there are no points on the plot, put the text in the center
      if (!show_drops) {
        text_pos_hex <- 0
        text_pos_fam <- 0
        hjust = 0.5
        vjust = 0.5
      }
      
      p <- p + 
        ggplot2::geom_text(
          data = dplyr::filter_(meta_used, ~ success),
          ggplot2::aes(label = paste0(mt_freq, "%")),
          x = text_pos_hex, y = text_pos_fam,
          hjust = hjust, vjust = vjust,
          fontface = "bold", size = mt_freq_size, show_guide = FALSE)
    }
  }
  
  p
}