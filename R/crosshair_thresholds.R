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
  
  params[['CLASSIFY']][['X_THRESHOLD']] <- 5000  # very arbitrary value
  params[['CLASSIFY']][['Y_THRESHOLD']] <- 5000
  
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


# 