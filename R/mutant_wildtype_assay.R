MUTANT_WILDTYPE_ASSAY <- "mutant_wildtype_assay"

parent_plate_type.mutant_wildtype_assay <- function(plate) {
  "ppnp_assay"
}

define_params.mutant_wildtype_assay <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME' = 'wildtype',
      'NEGATIVE_NAME' = 'mutant'
    )
  )
  params %<>% modifyList(new_params)
  
  params
}

#' @export
wells_wildtype <- function(x) {
  stopifnot(x %>% inherits("mutant_wildtype_assay"))
  wells_positive(x)
}

#' @export
wells_mutant <- function(x) {
  stopifnot(x %>% inherits("mutant_wildtype_assay"))
  wells_negative(x)
}

plot.mutant_wildtype_assay <- function(
  x,
  wells, samples,
  col_drops_mutant = "purple3", col_drops_wildtype = "green3",
  col_drops_rain = "black",
  show_mutant_freq = TRUE, text_size_mutant_freq = 4,
  alpha_drops_low_mutant_freq = 0.5,
  show_low_high_mut_freq = TRUE,
  bg_mutant = "purple3", bg_wildtype = "green3",
  alpha_bg_low_high_mut_freq = 0.1,
  ...
  )
{
  # call the plot function for general mutant/wildtype ddpcr plates
  # but use more user-friendly param names 
  NextMethod("plot", x,
             col_drops_negative = col_drops_mutant,
             col_drops_positive = col_drops_wildtype,
             col_drops_rain = col_drops_rain,
             show_negative_freq = show_mutant_freq,
             text_size_negative_freq = text_size_mutant_freq,
             alpha_drops_low_negative_freq = alpha_drops_low_mutant_freq,
             show_low_high_neg_freq = show_low_high_mut_freq,
             bg_negative = bg_mutant, bg_positive = bg_wildtype,
             alpha_bg_low_high_neg_freq = alpha_bg_low_high_mut_freq)
}