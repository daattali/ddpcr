#' @export
plot.ddpcr_plate <- function(
  plate,
  wells, samples,
  superimpose = FALSE, show_full_plate = FALSE,
  show_drops = TRUE, show_empty_drops = FALSE, show_outlier_drops = FALSE,
  show_failed_wells = TRUE,
  col_undefined = "black", col_failed = "black", col_empty = "black",
  col_outlier = "orange",
  bg_failed = "#111111", bg_unused = "#FFFFFF",
  alpha_drops = 0.1, alpha_drops_outlier = 1,
  alpha_bg_failed = 0.7,
  xlab, ylab, title,
  show_grid = FALSE, label_axes = FALSE)
{
  
  plate <- subset(plate, wells, samples)
  
  if (!show_failed_wells && status(plate) >= STATUS_FAILED_REMOVED) {
    plate %<>% subset(wells_success(.))
  }
  
  X_var <- params(plate, 'GENERAL', 'X_VAR')
  Y_var <- params(plate, 'GENERAL', 'Y_VAR')
  if (missing(xlab)) {
    xlab <- X_var
  }
  if (missing(ylab)) {
    ylab <- Y_var
  }
  
  meta <- plate_meta(plate)
  data <- plate_data(plate)
  
  # prepare the data to be plotted
  rownames(meta) <- meta[['well']]
  meta[['row']] %<>% as.factor
  meta[['col']] %<>% as.factor
  data[['row']] <- data[['well']] %>% get_row %>% as.factor
  data[['col']] <- data[['well']] %>% get_col %>% as.integer %>% as.factor
  data[['cluster']] %<>% as.factor
  
  if (!show_empty_drops) {
    data %<>% dplyr::filter_(~ cluster != CLUSTER_EMPTY)
  }
  if (!show_outlier_drops) {
    data %<>% dplyr::filter_(~ cluster != CLUSTER_OUTLIER)
  }
  
  if (plate %>% wells_used %>% length == 0) {
    err_msg("There are no wells to show.")
  }

  meta_used <- meta %>% dplyr::filter_(~ used)
  
  # define the colours of the clusters
  # Make sure the order matches the order of the CLUSTER_ constants
  cluster_cols <- c(col_undefined, col_failed, col_outlier, col_empty)
  
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
    show_drops <- TRUE
  } else {
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
        ggplot2::aes_string(x = X_var, y = Y_var, color = "cluster"),
        alpha = alpha_drops,
        show_guide = FALSE) +
      ggplot2::scale_color_manual(values = cluster_cols)

    if (show_outlier_drops) {
      p <- p +
        ggplot2::geom_point(
          data = data %>% dplyr::filter_(~ cluster == CLUSTER_OUTLIER),
          ggplot2::aes_string(X_var, Y_var),
          alpha = alpha_drops_outlier,
          col = col_outlier)
    }
  }

  # show the failed ddPCR runs
  if (show_failed_wells && !superimpose && status(plate) >= STATUS_FAILED_REMOVED) {
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
  
  # Done!
  p
}