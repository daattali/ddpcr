## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

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
#' @export
cluster_name <- function(plate, cluster) {
  cluster %>% as.integer
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
check_step <- function(plate, step, must_exist = FALSE) {
  exists <- plate %>% status >= step - 1
  if (must_exist && !exists) {
    err_msg("analysis is not at the required step")
  }
  exists
}

DEFAULT_PLATE_META <-
  expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
  magrittr::set_colnames(c("row", "col")) %>%
  dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                 "sample" = NA, "used" = FALSE) %>%
  dplyr::select_("well", "sample", "row", "col", "used")

