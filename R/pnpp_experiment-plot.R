## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plot a ddPCR plate of type PNPP experiment
#'
#' Same plot as \code{\link[ddpcr]{plot.ddpcr_plate}} but with a few extra
#' features that are specific to PNPP experiments The main additions are that
#' the negative frequency of each well can be written in each well, and well
#' background colours can be used to differentiate between wells with a
#' significant negative cluster vs wells with mostly positive drops. Take a look
#' at \code{\link[ddpcr]{plot.ddpcr_plate}} to see all supported parameters
#' and more information.
#'
#' @inheritParams plot.ddpcr_plate
#' @param col_drops_negative The colour to use for negative droplets.
#' See 'Droplet visibility options' for \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @param col_drops_positive The colour to use for positive droplets.
#' See 'Droplet visibility options' for \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @param col_drops_rain The colour to use for rain droplets.
#' See 'Droplet visibility options' for \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @param show_negative_freq If \code{TRUE}, show the negative frequency
#' as a percentage on each well.
#' @param text_size_negative_freq Text size of the printed negative frequencies.
#' @param alpha_drops_low_negative_freq Transparency of negative droplets
#' in wells with mostly positive droplets. In wells where there are very few
#' negative droplets, it might be useful to make them more visible by increasing
#' their transparency.
#' @param show_low_high_neg_freq Differentiate between wells with a high vs
#' low negative frequency by having a different background colour to the well.
#' @param bg_negative The background colour for wells that have a significant
#' negative cluster.
#' @param bg_positive The background colour for wells that have mostly positive
#' drops.
#' @param alpha_bg_low_high_neg_freq The transparency value for \code{bg_negative}
#' and \code{bg_positive}.
#' @param ... Parameters to pass to \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @return A ggplot2 plot object.
#' @seealso
#' \code{\link[ddpcr]{plot.ddpcr_plate}}\cr
#' \code{\link[ddpcr]{pnpp_experiment}}
#' @examples
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$pnpp_experiment)
#' positive_dim(plate) <- "Y"
#' plot(plate)
#' plate <- plate %>% analyze
#' plot(plate)
#' plot(plate, "A01:C05", col_drops_rain = "blue")
#' }
#' @export
#' @keywords internal
plot.pnpp_experiment <- function(
  x,
  wells, samples,
  ...,
  col_drops_negative = "purple3", col_drops_positive = "green3",
  col_drops_rain = "black",
  show_negative_freq = TRUE, text_size_negative_freq = 4,
  alpha_drops_low_negative_freq = 0.5,
  show_low_high_neg_freq = TRUE,
  bg_negative = "purple3", bg_positive = "green3",
  alpha_bg_low_high_neg_freq = 0.1,
  superimpose = FALSE, show_drops = TRUE, drops_size = 1)
{
  p <- NextMethod("plot",
                  col_drops_negative = col_drops_negative,
                  col_drops_positive = col_drops_positive,
                  col_drops_rain = col_drops_rain,
                  show_drops = show_drops, superimpose = superimpose,
                  drops_size = drops_size)
  plate <- subset(x, wells, samples)
  rm(x)

  # prepare the data to be plotted
  meta <- plate_meta(plate)
  meta[['row']] %<>% as.factor
  meta[['col']] %<>% as.factor
  meta_used <- meta %>% dplyr::filter(.data[["used"]])

  # if the drops have not been classified yet, we can't show negative freq
  if (status(plate) < step(plate, 'CLASSIFY')) {
    show_negative_freq = FALSE
    show_low_high_neg_freq = FALSE
  }

  # if we're superimposing all wells into one, then don't show negative freq
  if (superimpose) {
    show_drops = TRUE
    show_negative_freq = FALSE
    show_low_high_neg_freq = FALSE
  }

  # define the colours of the backgrounds of wells with high/low negative freq
  if (wells_positive(plate) %>% length > 0) {
    bg_cols <- c(bg_positive, bg_negative)
  } else {
    bg_cols <- c(bg_negative)
  }

  # extract the negative drops in mostly-positive wells
  neg_drops_low_freq <-
    plate %>%
    plate_data %>%
    dplyr::filter(.data[["well"]] %in% wells_positive(plate)) %>%
    dplyr::filter(.data[["cluster"]] == cluster(plate, 'NEGATIVE'))
  neg_drops_low_freq[['row']] <- substring(neg_drops_low_freq[['well']], 1, 1) %>% as.factor
  neg_drops_low_freq[['col']] <- substring(neg_drops_low_freq[['well']], 2, 3) %>% as.integer %>% as.factor

  # show the negative drops in mostly-positive wells
  if (show_drops) {
    if (neg_drops_low_freq %>% nrow > 0) {
      p <- p +
        ggplot2::geom_point(
          data = neg_drops_low_freq,
          ggplot2::aes_string(x_var(plate), y_var(plate)),
          alpha = alpha_drops_low_negative_freq,
          color = col_drops_negative,
          size = drops_size
        )
    }
  }

  # show mostly-positive vs significant-negative well as background colour
  if (show_low_high_neg_freq) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = meta_used %>% dplyr::filter(.data[["success"]]),
          ggplot2::aes_string(fill = meta_var_name(plate, "significant_negative_cluster"),
                              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = alpha_bg_low_high_neg_freq,
          show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = bg_cols)
    }
  }

  ggb <- ggplot2::ggplot_build(p)

  # show the negative frequency as text on each well
  if (show_negative_freq) {
    if (sum(meta_used[['success']], na.rm = TRUE) > 0) {

      # determine where on the plot to show the text, depending on where the drops are
      if (!show_drops) {
        text_pos_x <- 0
        text_pos_y <- 0
        hjust = 0.5
        vjust = 0.5
      } else if (positive_dim(plate) == "Y") {
        if (utils::packageVersion("ggplot2") > "2.2.1") {
          text_pos_x <- ggb$layout$panel_scales_x[[1]]$range$range[1] +
            diff(ggb$layout$panel_scales_x[[1]]$range$range) * 0.95
          text_pos_y <- ggb$layout$panel_scales_y[[1]]$range$range[1] +
            diff(ggb$layout$panel_scales_y[[1]]$range$range) * 0.1
        } else {
          text_pos_x <- ggb$layout$panel_ranges[[1]]$x.range[1] +
            diff(ggb$layout$panel_ranges[[1]]$x.range) * 0.95
          text_pos_y <- ggb$layout$panel_ranges[[1]]$y.range[1] +
            diff(ggb$layout$panel_ranges[[1]]$y.range) * 0.1
        }
        hjust = 1
        vjust = 0
      } else {
        if (utils::packageVersion("ggplot2") > "2.2.1") {
          text_pos_x <- ggb$layout$panel_scales_x[[1]]$range$range[1] +
            diff(ggb$layout$panel_scales_x[[1]]$range$range) * 0.1
          text_pos_y <- ggb$layout$panel_scales_y[[1]]$range$range[1] +
            diff(ggb$layout$panel_scales_y[[1]]$range$range) * 0.95
        } else {
          text_pos_x <- ggb$layout$panel_ranges[[1]]$x.range[1] +
            diff(ggb$layout$panel_ranges[[1]]$x.range) * 0.1
          text_pos_y <- ggb$layout$panel_ranges[[1]]$y.range[1] +
            diff(ggb$layout$panel_ranges[[1]]$y.range) * 0.95
        }
        hjust = 0
        vjust = 1
      }

      meta_used[['neg_freq_text']] <-
        paste0(meta_used[[meta_var_name(plate, 'negative_freq')]], "%")
      p <- p +
        ggplot2::geom_text(
          data = dplyr::filter(meta_used, .data[["success"]]),
          ggplot2::aes_string(label = "neg_freq_text"),
          x = text_pos_x, y = text_pos_y,
          hjust = hjust, vjust = vjust,
          fontface = "bold",
          size = text_size_negative_freq,
          show.legend = FALSE)
    }
  }

  p
}
