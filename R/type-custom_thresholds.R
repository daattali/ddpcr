## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: custom thresholds
#' 
#' The \code{custom_thresholds} plate type is used when you want to gate ddPCR
#' droplet data into four quadrants according to HEX and FAM values that you
#' manually set. All wells in the plate will use the same threshold values.
#' 
#' Plates with this type have only three analysis steps: \code{INITIALIZE},
#' \code{REMOVE_OUTLIERS}, and \code{CLASSIFY} (according to the custom thresholds).
#' 
#' Plates with this type have the following droplet clusters:
#' \code{UNDEFINED}, \code{OUTLIER}, \code{EMPTY} (bottom-left quadrant),
#' \code{X_POSITIVE} (bottom-right quadrant), \code{Y_POSITIVE} (top-left quadrant),
#' \code{BOTH_POSITIVE} (top-right quadrant).
#' 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}
#' \code{\link[ddpcr]{x_threshold}}
#' \code{\link[ddpcr]{y_threshold}}
#' \code{\link[ddpcr]{thresholds}}
#' @name custom_thresholds
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$custom_thresholds)
#' type(plate)
#' plate %>% analyze %>% plot
#' } 
NULL

plate_types[['custom_thresholds']] <- "custom_thresholds"

#' Define plate type parameters for custom thresholds plates
#' @inheritParams define_params
define_params.custom_thresholds <- function(plate) {
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

#' Define droplet clusters for custom thresholds plates
#' @inheritParams define_clusters
define_clusters.custom_thresholds <- function(plate) {
  c(
    'UNDEFINED',
    'OUTLIER',
    'EMPTY',
    'X_POSITIVE',
    'Y_POSITIVE',
    'BOTH_POSITIVE'
  )
}

#' Define analysis steps for custom thresholds plates
#' @inheritParams define_steps
define_steps.custom_thresholds <- function(plate) {
  list(
    'INITIALIZE'      = 'init_plate',
    'REMOVE_OUTLIERS' = 'remove_outliers',
    'CLASSIFY'        = 'classify_thresholds'
  )
}

#' Get/set the X threshold
#' 
#' For ddPCR plates of type \code{custom_thresholds}, get or set the threshold
#' along the X axis that divides the droplet quadrants.
#' @name x_threshold
#' @param plate A ddPCR plate.
#' @seealso 
#' \code{\link[ddpcr]{custom_thresholds}}
#' \code{\link[ddpcr]{y_threshold}}
#' \code{\link[ddpcr]{thresholds}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$custom_thresholds)
#' x_threshold(plate)
#' x_threshold(plate) <- 5500
#' plot(plate)
#' }
NULL

#' @rdname x_threshold
#' @return The current X threshold
#' @export
x_threshold <- function(plate) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  params(plate, 'CLASSIFY', 'X_THRESHOLD')
}
#' @rdname x_threshold
#' @param value The new X threshold
#' @export
`x_threshold<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value
  plate
}

#' Get/set the Y threshold
#' 
#' For ddPCR plates of type \code{custom_thresholds}, get or set the threshold
#' along the Y axis that divides the droplet quadrants.
#' @name y_threshold
#' @param plate A ddPCR plate.
#' @seealso 
#' \code{\link[ddpcr]{custom_thresholds}}
#' \code{\link[ddpcr]{x_threshold}}
#' \code{\link[ddpcr]{thresholds}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$custom_thresholds)
#' y_threshold(plate)
#' y_threshold(plate) <- 8000
#' plot(plate)
#' }
NULL

#' @rdname y_threshold
#' @return The current Y threshold
#' @export
y_threshold <- function(plate) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  params(plate, 'CLASSIFY', 'Y_THRESHOLD')
}
#' @rdname y_threshold
#' @param value The new Y threshold
#' @export
`y_threshold<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value
  plate
}

#' Get/set the thresholds
#' 
#' For ddPCR plates of type \code{custom_thresholds}, get or set the thresholds
#' that divide the four droplet quadrants.
#' @name thresholds
#' @param plate A ddPCR plate.
#' @param value The new thresholds as a 2-element numeric vector
#' @return The current thresholds
#' @seealso 
#' \code{\link[ddpcr]{custom_thresholds}}
#' \code{\link[ddpcr]{x_threshold}}
#' \code{\link[ddpcr]{y_threshold}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$custom_thresholds)
#' thresholds(plate)
#' thresholds(plate) <- c(5500, 8000)
#' set_thresholds(plate, c(5500, 8000))
#' }
NULL

#' @rdname thresholds
#' @export
thresholds <- function(plate) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  point2d(c(plate %>% x_threshold, plate %>% y_threshold))
}
#' @rdname thresholds
#' @export
`thresholds<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  value <- point2d(value)
  params(plate, 'CLASSIFY', 'X_THRESHOLD') <- value[1]
  params(plate, 'CLASSIFY', 'Y_THRESHOLD') <- value[2]
  plate
}
#' @rdname thresholds
#' @export
set_thresholds <- function(plate, value) {
  `thresholds<-`(plate, value)
}

classify_thresholds <- function(plate) {
  stopifnot(plate %>% inherits("custom_thresholds"))
  
  CURRENT_STEP <- plate %>% step('CLASSIFY')
  plate %>% check_step(CURRENT_STEP)  
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

#' Plot a ddPCR plate of type custom thresholds
#' 
#' Same plot as \code{\link[ddpcr]{plot.ddpcr_plate}} but with a few extra
#' features that are specific to plates with custom thresholds. Take a look
#' at \code{\link[ddpcr]{plot.ddpcr_plate}} to see all supported parameters
#' and more information.
#' 
#' @inheritParams plot.ddpcr_plate
#' @param show_thresholds If \code{TRUE}, show the thresholds.
#' @param col_thresholds The colour of the threshold lines.
#' @param show_drops_empty Whether or not to show the droplets defined as empty.
#' @param col_drops_x_positive The colour to use for droplets that are in the
#' X+Y- quadrant.
#' @param col_drops_y_positive The colour to use for droplets that are in the
#' X-Y+ quadrant.
#' @param col_drops_both_positive The colour to use for droplets that are in the
#' X+Y+ quadrant.
#' @param ... Parameters to pass to \code{\link[ddpcr]{plot.ddpcr_plate}}.
#' @return A ggplot2 plot object.
#' @seealso
#' \code{\link[ddpcr]{plot.ddpcr_plate}},
#' \code{\link[ddpcr]{custom_thresholds}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$custom_thresholds)
#' plate %>% set_thresholds(c(5500, 8000)) %>% analyze %>% plot
#' }
#' @export
plot.custom_thresholds <- function(
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
  
  # Show the custom thresholds
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