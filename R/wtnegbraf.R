#' @export
wells_mutant <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ has_mt_cluster) %>%
    .[['well']]
}

#' @export
wells_wildtype <- function(x) {
  stopifnot(x %>% inherits("wtnegbraf"))
  dplyr::filter_(x %>% plate_meta, ~ !has_mt_cluster) %>%
    .[['well']]
}