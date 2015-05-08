#' @export
plot.ppnp_assay <- function(
  x,
  wells, samples,
  col_drops_negative = "purple3", col_drops_positive = "green3",
  col_drops_rain = "black",
  show_negative_freq = TRUE, text_size_negative_freq = 4,
  alpha_drops_low_negative_freq = 0.5,
  show_low_high_neg_freq = TRUE,
  bg_negative = "purple3", bg_positive = "green3",
  alpha_bg_low_high_neg_freq = 0.1,
  superimpose = FALSE, show_drops = TRUE, drops_size = 2,
  ...)
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
          color = col_drops_negative,
          size = drops_size
        )
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