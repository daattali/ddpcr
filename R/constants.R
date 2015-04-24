# This file defines constants used in the package

STATUS_UNDEFINED               <- 0L
STATUS_INIT                    <- 1L
STATUS_FAILED_REMOVED          <- 2L
STATUS_OUTLIERS_REMOVED        <- 3L
STATUS_EMPTY_REMOVED           <- 4L

CLUSTER_UNDEFINED  <- 0L
CLUSTER_FAILED     <- 1L
CLUSTER_OUTLIER    <- 2L
CLUSTER_EMPTY      <- 3L

SEED <- 8

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
PARAMS_WELLSUCCESS['FAST'] <- TRUE
PARAMS_EMPTY <- list()
PARAMS_EMPTY['CUTOFF_SD'] <- 7
DEFAULT_PARAMS <- list(
  'GENERAL'           = PARAMS_GENERAL,
  'WELLSUCCESS'       = PARAMS_WELLSUCCESS,
  'OUTLIERS'          = PARAMS_OUTLIERS,
  'EMPTY'             = PARAMS_EMPTY
)