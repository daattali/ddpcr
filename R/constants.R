# This file defines constants used in the package

SEED <- 8

default_enums <- function(plate) {
  UseMethod("default_enums")
}

add_clusters <- function(enums, clusters) {
  add_enums(enums, 'CLUSTER', clusters)
}

add_steps <- function(enums, steps) {
  add_enums(enums, 'STEP', steps)
}

add_enums <- function(enums, type, values) {
  last_value <- enums[[type]] %>% length
  enums[[type]] %<>%
    c(setNames(seq(last_value + 1, last_value + length(values)), values))
  enums
}

step <- function(plate, step) {
  enums(plate)[['STEP']][[step]]
}
step_name <- function(plate, step) {
  enums(plate)[['STEP']][step] %>% names
}

cluster <- function(plate, cluster) {
  enums(plate)[['CLUSTER']][[cluster]]
}
cluster_name <- function(plate, cluster) {
  enums(plate)[['CLUSTER']][cluster] %>% names
}


enums <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['enums']]
}

`enums<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  plate[['enums']] <- value
  plate
}

set_default_enums <- function(plate) {
  enums(plate) <- default_enums(plate)
  plate
}

default_enums.ddpcr_plate <- function(plate) {
  list() %>%
    add_clusters(c(
      'UNDEFINED',
      'FAILED',
      'OUTLIER',
      'EMPTY'
    )) %>%
    add_steps(c(
      'UNDEFINED',
      'INIT',
      'FAILED_REMOVED',
      'OUTLIERS_REMOVED',
      'EMPTY_REMOVED'
    ))
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