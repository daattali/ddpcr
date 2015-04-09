# This file defines constants used in the package

STATUS_UNDEFINED               <- 0
STATUS_INIT                    <- 1
STATUS_LOADED                  <- 2
STATUS_OUTLIERS_REMOVED        <- 3
STATUS_FAILED_REMOVED          <- 4
STATUS_EMPTY_REMOVED           <- 5
STATUS_DROPLETS_CLASSIFIED     <- 6
STATUS_DROPLETS_RECLASSIFIED   <- 7

CLUSTER_UNDEFINED  <- 0
CLUSTER_EMPTY      <- 1
CLUSTER_WT         <- 2
CLUSTER_MT         <- 3
CLUSTER_RAIN       <- 4

SEED <- 8

PLATE_INFO <-
  expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
  magrittr::set_colnames(c("row", "col")) %>%
  dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col))