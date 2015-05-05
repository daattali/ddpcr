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

x_threshold <- function(plate) {
  params(plate, 'CLASSIFY', 'X_THRESHOLD')
}
`x_threshold<-` <- function(plate, value) {
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value
  plate
}

y_threshold <- function(plate) {
  params(plate, 'CLASSIFY', 'Y_THRESHOLD')
}
`y_threshold<-` <- function(plate, value) {
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value
  plate
}
thresholds <- function(plate) {
  point2d(c(plate %>% x_threshold, plate %>% y_threshold))
}
`thresholds<-` <- function(plate, value) {
  value <- point2d(value)
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value[1]
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value[2]
  plate
}

classify_thresholds <- function(plate) {
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
  
  status(plate) <- CURRENT_STEP
  step_end()
  
  plate
}


#' @export
plot.crosshair_thresholds <- function(
  x,
  wells, samples, show_drops_empty = TRUE,
  show_thresholds = TRUE,
  col_thresholds = "black",
  ...)
{
  # Plot a regular ddpcr plate
  p <- NextMethod("plot", x, show_drops_empty = show_drops_empty)
  
  # Show the crosshair thresholds
  if (show_thresholds) {
    x <- subset(x, wells, samples)
    meta <- plate_meta(x)
    p <- p +
      geom_hline(data = dplyr::filter_(meta, ~ used),
                 aes(yintercept = y_threshold(x)),
                 color = col_thresholds) +
      geom_vline(data = dplyr::filter_(meta, ~ used),
                 aes(xintercept = x_threshold(x)),
                 color = col_thresholds)
  }
  
  p
}