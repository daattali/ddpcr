## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Quickly plot a ddPCR plate with hex-based binning
#' 
#' Plot the data of a ddPCR plate. A plate can be plotted throughout any stage
#' of the analysis, and the most up-to-date data will be shown. For example,
#' a plot performed after initializing a plat will show all the raw data, but
#' a plot performed after analyzing a plate will show information such as failed
#' wells.
#' 
#' @param x A ddPCR plate.
#' @param wells Only plot selected wells. Supports range notation, see
#' \code{\link[ddpcr]{subset.ddpcr_plate}}.
#' @param samples Only plot selected samples.
#' @param superimpose If \code{TRUE}, show all wells superimposed in one plot;
#' otherwise, show wells in a grid.
#' @param show_full_plate If \code{TRUE}, show full 96-well plate; otherwise,
#' show only plate rows and columns that are used.
#' @param show_failed_wells Whether or not to include wells that are deemed
#' as failed ddPCR runs.
#' @param bg_plot The background colour for the plot.
#' @param bg_failed The background colour to use for failed wells.
#' @param bg_unused The background colour to use for unused wells.
#' @param alpha_bg_failed The transparency of the background of failed wells.
#' @param xlab The label on the X axis.
#' @param ylab The label on the Y axis.
#' @param title The title for the plot.
#' @param show_grid Whether or not to show grid lines.
#' @param show_grid_labels Whether or not to show numeric labels for the grid
#' lines along the axes.
#' @param text_size_title Text size of the title.
#' @param text_size_row_col Text size of the row and column labels.
#' @param text_size_axes_labels Text size of the X/Y axis labels.
#' @param text_size_grid_labels Text size of the numeric grid line labels.
#' @param ... Ignored.
#' @return A ggplot2 plot object.
#' 
#' @section Extending ddpcr_plate:
#' If you create your own plate type, this default hex plot function might be
#' enough if there is no extra information you want to display in a plot.
#' If you do need to provide a more customized plot function, it can be
#' a good idea to use the output from this plot function as a basis and only
#' add the code that is necessary to append to the plot. 
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir())
#' hex_plot(plate)
#' plate <- plate %>% analyze
#' hex_plot(plate)
#' }
#' @export
hex_plot.ddpcr_plate <- function(
  x,
  wells, samples,
  superimpose = FALSE, show_full_plate = FALSE,
  show_failed_wells = TRUE, alpha_bg_failed = 0.7,
  bg_plot = "transparent", bg_failed = "#111111", bg_unused = "#FFFFFF",
  xlab = x_var(plate), ylab = y_var(plate), title = NULL,
  show_grid = FALSE, show_grid_labels = FALSE,
  text_size_title = 14, text_size_row_col = 12, text_size_axes_labels = 12, 
  text_size_grid_labels = 12, ...) {

  # only keep the requested wells/samples
  plate <- subset(x, wells, samples)
  rm(x)
  
  # don't show failed wells
  if (!show_failed_wells &&
      plate %>% has_step('REMOVE_FAILURES') &&
      status(plate) >= step(plate, 'REMOVE_FAILURES')) {
    plate %<>% subset(wells_success(.))
  }
  
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  meta <- plate_meta(plate)
  data <- plate_data(plate)
  
  # make a list of all parameters available, including default and ... params
  all_params <- c(as.list(environment()), list(...))  
  
  # prepare the data to be plotted
  meta[['row']] %<>% as.factor
  meta[['col']] %<>% as.factor
    
  data[['row']] <- data[['well']] %>% get_row %>% as.factor
  data[['col']] <- data[['well']] %>% get_col %>% as.integer %>% as.factor

  meta_used <- meta %>% dplyr::filter_(~ used)
  
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
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 90, vjust = 0.5),
      title            = ggplot2::element_text(size = text_size_title),
      axis.title       = ggplot2::element_text(size = text_size_axes_labels),
      axis.text        = ggplot2::element_text(size = text_size_grid_labels),
      strip.text       = ggplot2::element_text(size = text_size_row_col),
      plot.background  = ggplot2::element_rect(fill = bg_plot, color = bg_plot),
      aspect.ratio     = 1
    )
  
  # superimpose all the data from all the wells onto one plot instead of a grid
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
  
  # plot the droplets
  p <- p +
   ggplot2::geom_hex(
     data = data,
     ggplot2::aes_string(x = x_var, y = y_var),
     show.legend = FALSE, 
     bins = 100) +    
     ggplot2::scale_fill_gradientn(colors = 
       rev(RColorBrewer::brewer.pal(11, "Spectral")), 
       name = "Drops")

  # show the failed ddPCR runs
  if (show_failed_wells && !superimpose && 
      plate %>% has_step('REMOVE_FAILURES') &&
      status(plate) >= step(plate, 'REMOVE_FAILURES')) {
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
  if (!show_grid_labels) {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  }
  
  # attach information about how many rows and columns are displayed
  rows <- meta[['row']] %>% unique %>% length
  cols <- meta[['col']] %>% unique %>% length
  if (superimpose) {
    rows <- 3
    cols <- 3
  }
  attr(p, 'ddpcr_rows') <- rows
  attr(p, 'ddpcr_cols') <- cols
  
  # Done!
  p
}

## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Quickly plot a ddPCR plate with hex-based binning
#' 
#' Plot the data of a ddPCR plate. A plate can be plotted throughout any stage
#' of the analysis, and the most up-to-date data will be shown. For example,
#' a plot performed after initializing a plat will show all the raw data, but
#' a plot performed after analyzing a plate will show information such as failed
#' wells.
#' 
#' @param x A ddPCR plate.
#' @param wells Only plot selected wells. Supports range notation, see
#' \code{\link[ddpcr]{subset.ddpcr_plate}}.
#' @param samples Only plot selected samples.
#' @param superimpose If \code{TRUE}, show all wells superimposed in one plot;
#' otherwise, show wells in a grid.
#' @param show_full_plate If \code{TRUE}, show full 96-well plate; otherwise,
#' show only plate rows and columns that are used.
#' @param show_failed_wells Whether or not to include wells that are deemed
#' as failed ddPCR runs.
#' @param bg_plot The background colour for the plot.
#' @param bg_failed The background colour to use for failed wells.
#' @param bg_unused The background colour to use for unused wells.
#' @param alpha_bg_failed The transparency of the background of failed wells.
#' @param xlab The label on the X axis.
#' @param ylab The label on the Y axis.
#' @param title The title for the plot.
#' @param show_grid Whether or not to show grid lines.
#' @param show_grid_labels Whether or not to show numeric labels for the grid
#' lines along the axes.
#' @param text_size_title Text size of the title.
#' @param text_size_row_col Text size of the row and column labels.
#' @param text_size_axes_labels Text size of the X/Y axis labels.
#' @param text_size_grid_labels Text size of the numeric grid line labels.
#' @param ... Ignored.
#' @return A ggplot2 plot object.
#' 
#' @section Extending ddpcr_plate:
#' If you create your own plate type, this default hex plot function might be
#' enough if there is no extra information you want to display in a plot.
#' If you do need to provide a more customized plot function, it can be
#' a good idea to use the output from this plot function as a basis and only
#' add the code that is necessary to append to the plot. 
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir())
#' hex_plot(plate)
#' plate <- plate %>% analyze
#' hex_plot(plate)
#' }
#' @export 
hex_plot <- function(  x,
   wells, samples,
   superimpose = FALSE, show_full_plate = FALSE,
   show_failed_wells = TRUE, alpha_bg_failed = 0.7,
   bg_plot = "transparent", bg_failed = "#111111", bg_unused = "#FFFFFF",
   xlab = x_var(plate), ylab = y_var(plate), title = NULL,
   show_grid = FALSE, show_grid_labels = FALSE,
   text_size_title = 14, text_size_row_col = 12, text_size_axes_labels = 12, 
   text_size_grid_labels = 12, ...) {
   
   UseMethod("hex_plot")
}