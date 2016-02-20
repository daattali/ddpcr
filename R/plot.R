## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plot a ddPCR plate
#' 
#' Plot the data of a ddPCR plate. A plate can be plotted throughout any stage
#' of the analysis, and the most up-to-date data will be shown. For example,
#' a plot performed after initializing a plat will show all the raw data, but
#' a plot performed after analyzing a plate will show information such as
#' empty drops and failed wells.
#' 
#' @param x A ddPCR plate.
#' @param wells Only plot selected wells. Supports range notation, see
#' \code{\link[ddpcr]{subset.ddpcr_plate}}.
#' @param samples Only plot selected samples.
#' @param superimpose If \code{TRUE}, show all wells superimposed in one plot;
#' otherwise, show wells in a grid.
#' @param show_full_plate If \code{TRUE}, show full 96-well plate; otherwise,
#' show only plate rows and columns that are used.
#' @param show_drops Whether or not to show the droplets. Setting to \code{FALSE}
#' is not useful if the droplets are the only thing shown in the plot, but it
#' can be useful if there is other information depicated in the plot, such as
#' any background colours or text that may appear in each well.
#' @param show_drops_empty Whether or not to show the droplets defined as empty.
#' See 'Droplet visibility options' below.
#' @param show_drops_outlier Whether or not to show the droplets defined as
#' outliers. See 'Droplet visibility options' below.
#' @param show_failed_wells Whether or not to include wells that are deemed
#' as failed ddPCR runs.
#' @param col_drops The default colour to use for any droplet.
#' @param col_drops_undefined The colour to use for droplets that have not been
#' analyzed yet. See 'Droplet visibility options' below.
#' @param col_drops_failed The colour to use for droplets in failed wells.
#' See 'Droplet visibility options' below.
#' @param col_drops_empty The colour to use for empty droplets.
#' See 'Droplet visibility options' below.
#' @param col_drops_outlier The colour to use for outlier droplets.
#' See 'Droplet visibility options' below.
#' @param bg_plot The background colour for the plot.
#' @param bg_failed The background colour to use for failed wells.
#' @param bg_unused The background colour to use for unused wells.
#' @param alpha_drops The transparency of droplets.
#' @param alpha_drops_outlier The transparency of outlier droplets.
#' See 'Droplet visibility options' below.
#' @param alpha_bg_failed The transparency of the background of failed wells.
#' @param xlab The label on the X axis.
#' @param ylab The label on the Y axis.
#' @param title The title for the plot.
#' @param show_grid Whether or not to show grid lines.
#' @param show_grid_labels Whether or not to show numeric labels for the grid
#' lines along the axes.
#' @param drops_size Size of droplets.
#' @param text_size_title Text size of the title.
#' @param text_size_row_col Text size of the row and column labels.
#' @param text_size_axes_labels Text size of the X/Y axis labels.
#' @param text_size_grid_labels Text size of the numeric grid line labels.
#' @param ... Ignored.
#' @return A ggplot2 plot object.
#' 
#' @section Droplet visibility options:
#' To make it easier to support any plate type with any types of droplet
#' clusters, there are three categories of special parameters that can always
#' be used:
#' 
#' \itemize{
#'   \item{\code{show_drops_*}}{ Whether or not to show a specific group of
#'   droplets.}
#'   \item{\code{col_drops_*}}{ What colour to use for a specific group of
#'   droplets.}
#'   \item{\code{alpha_drops_*}}{ What transparency to use for a specific group
#'   of droplets.}
#' }
#' 
#' The \code{*} in the parameter name can be replaced by the name of any
#' droplet cluster. Use the \code{\link[ddpcr]{clusters}} function to 
#' find out what clusters the droplets in a plate can be assigned to.
#' 
#' For example, the default clusters that exist in a plain \code{ddpcr_plate}
#' are "UNDEFINED", "FAILED", "OUTLIER", and "EMPTY".  This means that if you
#' want to hide the empty drops and make the transparency of drops in failed
#' wells 0.5, you could add the two parameters \code{show_drops_empty = FALSE}
#' and \code{alpha_drops_failed = 0.5}. Note that letter case is not important.
#' If another plate type defines a new clsuter of type "MUTANT" and you want to
#' show these drops in red, you can add the parameter
#' \code{col_drops_mutant = "red"}.
#' 
#' Note that some of the more common combinations of these parameters are
#' defined by default (for example, \code{col_drops_failed} is defined in the
#' list of parameters), but these three parameter categories will work for
#' any cluster type.
#' 
#' @section Extending ddpcr_plate:
#' If you create your own plate type, this default plot function might be
#' enough if there is no extra information you want to display in a plot.
#' If you do need to provide a more customized plot function, it can be
#' a good idea to use the output from this plot function as a basis and only
#' add the code that is necessary to append to the plot.  See
#' \code{\link[ddpcr]{plot.custom_thresholds}} as an example of how to
#' extend this plot function.
#' @examples 
#' \dontrun{
#' plate <- new_plate(sample_data_dir())
#' plot(plate)
#' plate <- plate %>% analyze
#' plot(plate)
#' plot(plate, "A01:C05", show_drops_empty = TRUE, col_drops_empty = "red")
#' }
#' @export
plot.ddpcr_plate <- function(
  x,
  wells, samples,
  superimpose = FALSE, show_full_plate = FALSE,
  show_drops = TRUE, show_drops_empty = FALSE, show_drops_outlier = FALSE,
  show_failed_wells = TRUE,
  col_drops = "black", col_drops_undefined = col_drops, col_drops_failed = col_drops,
  col_drops_empty = col_drops, col_drops_outlier = "orange",
  bg_plot = "transparent", bg_failed = "#111111", bg_unused = "#FFFFFF",
  alpha_drops = 0.2, alpha_drops_outlier = 1,
  alpha_bg_failed = 0.7,
  xlab = x_var(plate), ylab = y_var(plate), title = NULL,
  show_grid = FALSE, show_grid_labels = FALSE,
  drops_size = 1,
  text_size_title = 14, text_size_row_col = 12, text_size_axes_labels = 12, 
  text_size_grid_labels = 12,
  ...)
{
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
  
  if (show_drops) {
    data[['row']] <- data[['well']] %>% get_row %>% as.factor
    data[['col']] <- data[['well']] %>% get_col %>% as.integer %>% as.factor
    data[['cluster']] %<>% as.factor
    
    # Remove drops that we don't want to show
    visible_clusters <-
      data[['cluster']] %>%
      unique %>%
      as.character %>%
      as.numeric %>%
      sort %>%
      cluster_name(plate, .)
    show_clusters <- 
      vapply(visible_clusters,
             function(cluster) {
               param_name <- paste0("show_drops_", cluster) %>% tolower
               param_idx <- which(param_name == all_params %>% names %>% tolower)
               if (length(param_idx) > 0) {
                 show_cluster <- as.logical(all_params[[param_idx %>% utils::tail(1)]])
               } else {
                 show_cluster <- TRUE
               }
               show_cluster
             },
             logical(1)
      )  
    data %<>% dplyr::filter_(~ show_clusters[cluster]) %>% droplevels
  }
    
  # make sure after removing unwanted drops/wells, we still have something to show
  if (plate %>% wells_used %>% length == 0) {
    err_msg("There are no wells to show.")
  }

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
    
    # define the colour of each droplet
    visible_clusters <- data[['cluster']] %>% unique %>% as.character %>% as.numeric %>% sort
    cluster_cols <- 
      vapply(visible_clusters,
             function(cluster) {
               cluster_name <- cluster_name(plate, cluster) %>% tolower
               param_name <- paste0("col_drops_", cluster_name)
               param_idx <- which(param_name == all_params %>% names %>% tolower)
               if (length(param_idx) > 0) {
                 cluster_col <- all_params[[param_idx %>% utils::tail(1)]]
               } else {
                 cluster_col <- col_drops
               }
               cluster_col
            },
            character(1)
      )
    
    # Find out what alpha (transparency) value to give each cluster
    cluster_alphas <- 
      vapply(visible_clusters,
             function(cluster) {
               cluster_name <- cluster_name(plate, cluster) %>% tolower
               param_name <- paste0("alpha_drops_", cluster_name)
               param_idx <- which(param_name == all_params %>% names %>% tolower)
               if (length(param_idx) > 0) {
                 cluster_alpha <- all_params[[param_idx %>% utils::tail(1)]]
               } else {
                 cluster_alpha <- alpha_drops
               }
               cluster_alpha
             },
             numeric(1)
      )

    # plot the droplets
    p <- p +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes_string(x = x_var, y = y_var,
                            color = "cluster", alpha = "cluster"),
        size = drops_size,
        show.legend = FALSE) +
      ggplot2::scale_color_manual(values = cluster_cols) +
      ggplot2::scale_alpha_manual(values = cluster_alphas)
    }

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
  
  # so that R won't complain when we don't show the drops
  if (!show_drops) {
    p <- p +
      ggplot2::geom_text(data = meta, ggplot2::aes(0, 0, label = ""))
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
