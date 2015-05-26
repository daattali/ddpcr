## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' Calculate negative frequency based on number of drops
#' @param negative_drops Number of negative drops.
#' @param positive_drops Number of positive drops.
#' @return Fraction of drops that are negative.
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}}
#' @examples 
#' calc_negative_freq_simple(5, 45)
#' @keywords internal
#' @export
calc_negative_freq_simple <- function(negative_drops, positive_drops) {
  (negative_drops / (negative_drops + positive_drops)) %>%
    magrittr::multiply_by(100) %>%
    signif(3)
}

#' Calculate negative frequency of a single well
#' @param plate A ddPCR plate
#' @param well_id A well ID
#' @return list with 3 elemnts: number of negative drops, number of positive
#' drops, and fraction of negative drops.
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}}
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% calculate_neg_freq_single("B06")
#' @keywords internal
#' @export
calculate_neg_freq_single <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, clusters = TRUE)
  
  negative_num <- (well_data[['cluster']] == plate %>% cluster('NEGATIVE')) %>% sum
  positive_num <- (well_data[['cluster']] == plate %>% cluster('POSITIVE')) %>% sum
  negative_freq <- calc_negative_freq_simple(negative_num, positive_num)
  
  list(negative_num = negative_num,
       positive_num = positive_num,
       negative_freq = negative_freq)
}

#' Calculate negative frequencies in whole plate
#' 
#' The resulting plate has the same droplet data but an updated metadata with
#' the number of negative/positive droplets and the negative frequency.
#' @param plate A ddPCR plate
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}}
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% calculate_negative_freqs %>%
#'   well_info(wells_success(plate), "negative_freq")
#' @keywords internal
#' @export
calculate_negative_freqs <- function(plate) {
  negative_freqs <-
    vapply(wells_success(plate),
           function(x) calculate_neg_freq_single(plate, x),
           vector(mode = "list", length = 3)) %>%
    lol_to_df %>%
    magrittr::set_names(lapply(names(.), function(x) meta_var_name(plate, x)))
  
  plate_meta(plate) %<>%
    merge_dfs_overwrite_col(negative_freqs)
  
  plate
}
