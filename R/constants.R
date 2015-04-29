# This file defines constants used in the package

SEED <- 8

define_clusters.ddpcr_plate <- function(plate) {
  c() %>%
    add_clusters(c(
      'UNDEFINED',
      'FAILED',
      'OUTLIER',
      'EMPTY'
    ))
}

add_clusters <- function(existing, new) {
  c(existing, new)
}

cluster <- function(plate, cluster) {
  res <- plate %>% clusters %>% {which(. == cluster)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find cluster `%s`", cluster))
  }
  res
}
cluster_name <- function(plate, cluster) {
  cluster %<% as.integer
  if (cluster < 1 || cluster > plate %>% clusters %>% length) {
    err_msg(sprintf("invalid cluster number: %s", cluster))
  }
  plate %>% clusters %>% .[cluster]
}

unanalyzed_clusters <- function(plate, current) {
  res <- plate %>% cluster('UNDEFINED')
  if (!missing(current)) {
    if (!current %>% is.numeric) {
      current <- cluster(plate, current)
    }
    res %<>%
      c(seq(current, plate %>% clusters %>% length))
  }
  res
}

define_steps.ddpcr_plate <- function(plate) {
  c() %>%
    add_steps(list(
      'INITIALIZE' = 'init_plate',
      'REMOVE_FAILURES' = 'remove_failures',
      'REMOVE_OUTLIERS' = 'remove_outliers',
      'REMOVE_EMPTY' = 'remove_empty'
    ))
}
add_steps <- function(existing, new) {
  c(existing, new)
}
step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  if (res %>% length != 1) {
    err_msg(sprintf("could not find step `%s`", step))
  }
  res
}
step_name <- function(plate, step) {
  step %<>% as.integer
  if (step < 1 || step > plate %>% steps %>% length) {
    err_msg(sprintf("invalid step number: %s", step))
  }
  plate %>% steps %>% names %>% .[step]
}
check_step <- function(plate, step) {
  stopifnot(plate %>% status >= step - 1)
}

DEFAULT_PLATE_META <-
  expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
  magrittr::set_colnames(c("row", "col")) %>%
  dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                 "sample" = NA, "used" = FALSE) %>%
  dplyr::select_("well", "sample", "row", "col", "used")

# Each parameter has a somewhat descriptive name of what it is used for, and
# all parameters used by a single step in the pipeline are in a list together
PARAMS_GENERAL <- list()
PARAMS_GENERAL['X_VAR'] <- "Channel_2"
PARAMS_GENERAL['Y_VAR'] <- "Channel_1"
PARAMS_GENERAL['DROPLET_VOLUME'] <- 0.91e-3
PARAMS_OUTLIERS <- list()
PARAMS_OUTLIERS['TOP_PERCENT'] <- 1
PARAMS_OUTLIERS['CUTOFF_IQR'] <- 5
PARAMS_WELLSUCCESS <- list()
PARAMS_WELLSUCCESS['TOTAL_DROPS_T'] <- 5000
PARAMS_WELLSUCCESS['NORMAL_LAMBDA_LOW_T'] <- 0.3
PARAMS_WELLSUCCESS['NORMAL_LAMBDA_HIGH_T'] <- 0.99
PARAMS_WELLSUCCESS['NORMAL_SIGMA_T'] <- 200
PARAMS_EMPTY <- list()
PARAMS_EMPTY['CUTOFF_SD'] <- 7
DEFAULT_PARAMS <- list(
  'GENERAL'           = PARAMS_GENERAL,
  'WELLSUCCESS'       = PARAMS_WELLSUCCESS,
  'OUTLIERS'          = PARAMS_OUTLIERS,
  'EMPTY'             = PARAMS_EMPTY
)