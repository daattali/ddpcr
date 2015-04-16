# TODO show outliers (and dont actually remove outliers)
#' @export
plot.ddpcr_plate <- function(
  plate,
  wells, samples,
  superimpose = FALSE, show_full_plate = FALSE,
  show_drops = TRUE, show_empty = FALSE, show_outliers = FALSE,
  show_failed = TRUE, show_mt_vs_wt = TRUE,
  show_mt_freq = TRUE, mt_freq_size = 4,
  col_failed = "black", col_empty = "black",
  col_wt = "green", col_mt = "purple", col_rain = "black", col_outlier = "orange",
  bg_failed = "#111111", bg_unused = "#FFFFFF", bg_wt = "green", bg_mt = "purple",
  alpha_drops = 0.1, alpha_drops_low_mt = 0.5,
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
    #TODO
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
  cluster_cols <- c(col_failed, col_empty, col_wt, col_mt, col_rain)
  
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