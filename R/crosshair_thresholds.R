## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' @export
CROSSHAIR_THRESHOLDS <- "crosshair_thresholds"

define_clusters.crosshair_thresholds <- function(plate) {
  c() %>%
    add_clusters(c(
      'UNDEFINED',
      'OUTLIER',
      'EMPTY',
      'X_POSITIVE',
      'Y_POSITIVE',
      'BOTH_POSITIVE'
    ))
}

define_steps.crosshair_thresholds <- function(plate) {
  c() %>%
    add_steps(list(
      'INITIALIZE'      = 'init_plate',
      'REMOVE_OUTLIERS' = 'remove_outliers',
      'CLASSIFY'        = 'classify_thresholds'
    ))
}

define_params.crosshair_thresholds <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'CLASSIFY' = list(
      'X_THRESHOLD' = 5000,   # very arbitrary value
      'Y_THRESHOLD' = 5000
    )
  )
  params %<>% modifyList(new_params)
  
  params
}

#' @export
x_threshold <- function(plate) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  params(plate, 'CLASSIFY', 'X_THRESHOLD')
}
#' @export
`x_threshold<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value
  plate
}

#' @export
y_threshold <- function(plate) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  params(plate, 'CLASSIFY', 'Y_THRESHOLD')
}
#' @export
`y_threshold<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value
  plate
}
thresholds <- function(plate) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  point2d(c(plate %>% x_threshold, plate %>% y_threshold))
}
`thresholds<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  value <- point2d(value)
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value[1]
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value[2]
  plate
}
#' @export
set_thresholds <- function(plate, value) {
  `thresholds<-`(plate, value)
}

classify_thresholds <- function(plate) {
  stopifnot(plate %>% inherits("crosshair_thresholds"))
  
  CURRENT_STEP <- plate %>% step('CLASSIFY')
  plate %>% check_step(CURRENT_STEP, TRUE)  
  step_begin("Classifying droplets")
  
  x_threshold <- plate %>% x_threshold
  y_threshold <- plate %>% y_threshold
  data <- plate_data(plate)
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  CLUSTERS_UNANALYZED <- unanalyzed_clusters(plate, 'EMPTY')
  
  unanalyzed_idx <- data[['cluster']] %in% CLUSTERS_UNANALYZED
  ypos_idx <-
    unanalyzed_idx &
    data[[y_var]] >= y_threshold
  bothpos_idx <- 
    unanalyzed_idx &
    data[[x_var]] >= x_threshold
  xpos_idx <-
    unanalyzed_idx &
    data[[y_var]] < y_threshold
  empty_idx <-
    unanalyzed_idx &
    data[[x_var]] < x_threshold &
    data[[y_var]] < y_threshold

  data[ypos_idx, 'cluster'] <- plate %>% cluster('Y_POSITIVE')
  data[bothpos_idx, 'cluster'] <- plate %>% cluster('BOTH_POSITIVE')
  data[xpos_idx, 'cluster'] <- plate %>% cluster('X_POSITIVE')
  data[empty_idx, 'cluster'] <- plate %>% cluster('EMPTY')
  
  plate_data(plate) <- data
  
  # record how many drops are in each quadrant
  drops_per_quadrant <- 
    plyr::ddply(data, ~ well, function(x) {
      data.frame(
        'drops_empty' = sum(x[['cluster']] == cluster(plate, 'EMPTY')),
        'drops_x_positive' = sum(x[['cluster']] == cluster(plate, 'X_POSITIVE')),
        'drops_y_positive' = sum(x[['cluster']] == cluster(plate, 'Y_POSITIVE')),
        'drops_both_positive' = sum(x[['cluster']] == cluster(plate, 'BOTH_POSITIVE'))
      )
    })
  plate_meta(plate) %<>%
    dplyr::left_join(drops_per_quadrant, by = "well")
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}


#' @export
plot.crosshair_thresholds <- function(
  x,
  wells, samples,
  ...,
  show_thresholds = TRUE,
  col_thresholds = "black",
  show_drops_empty = TRUE,
  col_drops_x_positive = "green3",
  col_drops_y_positive = "blue",
  col_drops_both_positive = "orange"
  )
{
  # Plot a regular ddpcr plate
  p <- NextMethod("plot", x,
                  show_drops_empty = show_drops_empty,
                  col_drops_x_positive = col_drops_x_positive,
                  col_drops_y_positive = col_drops_y_positive,
                  col_drops_both_positive = col_drops_both_positive)
  
  # Show the crosshair thresholds
  if (show_thresholds) {
    x <- subset(x, wells, samples)
    meta <- plate_meta(x)
    meta[['x_threshold']] <- x_threshold(x)
    meta[['y_threshold']] <- y_threshold(x)
    p <- p +
      ggplot2::geom_hline(
        data = dplyr::filter_(meta, ~ used),
        ggplot2::aes_string(yintercept = "y_threshold"),
        color = col_thresholds
      ) +
      ggplot2::geom_vline(
        data = dplyr::filter_(meta, ~ used),
        ggplot2::aes_string(xintercept = "x_threshold"),
        color = col_thresholds
      )
  }
  
  p
}