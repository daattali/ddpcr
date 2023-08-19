## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: wildtype/mutant PNPP
#'
#' A plate of type \code{wildtype_mutant_pnpp} is a subtype of
#' \code{\link[ddpcr]{pnpp_experiment}} that assumes the double-positive cluster
#' denotes wildtype and the other non-empty cluster denotes mutant droplets.
#' There are two plate types that are subtypes of \code{wildtype_mutant_pnpp}:
#' \code{\link[ddpcr]{fam_positive_pnpp}} and \code{\link[ddpcr]{hex_positive_pnpp}}.
#' It is not recommended to use this type directly; instead you should use one
#' of the subtypes.
#'
#' Plates with this type have the following analysis steps: \code{INITIALIZE},
#' \code{REMOVE_FAILURES}, \code{REMOVE_OUTLIERS}, \code{REMOVE_EMPTY},
#' \code{CLASSIFY}, \code{RECLASSIFY}.
#'
#' Plates with this type have the following droplet clusters:
#' \code{UNDEFINED}, \code{FAILED}, \code{OUTLIER}, \code{EMPTY} (double-negative),
#' \code{RAIN} (not empty but not wildtype nor negative), \code{POSITIVE} (wildtype),
#' \code{NEGATIVE} (mutant).
#'
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on plate types.
#'
#' @seealso
#' \code{\link[ddpcr]{plate_types}}\cr
#' \code{\link[ddpcr]{fam_positive_pnpp}}\cr
#' \code{\link[ddpcr]{hex_positive_pnpp}}\cr
#' \code{\link[ddpcr]{pnpp_experiment}}\cr
#' \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{remove_failures}}\cr
#' \code{\link[ddpcr]{remove_outliers}}\cr
#' \code{\link[ddpcr]{remove_empty}}\cr
#' \code{\link[ddpcr]{classify_droplets}}\cr
#' \code{\link[ddpcr]{reclassify_droplets}}
#' @name wildtype_mutant_pnpp
#' @examples
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$wildtype_mutant_pnpp)
#' type(plate)
#' }
NULL

plate_types[['wildtype_mutant_pnpp']] <- "wildtype_mutant_pnpp"

#' Parent plate type of wildtype/mutant PNPP
#' @inheritParams parent_plate_type
#' @export
#' @keywords internal
parent_plate_type.wildtype_mutant_pnpp <- function(plate) {
  "pnpp_experiment"
}

#' Define plate type parameters for wildtype/mutant PNPP
#' @inheritParams define_params
#' @export
#' @keywords internal
define_params.wildtype_mutant_pnpp <- function(plate) {
  params <- NextMethod("define_params")

  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME' = 'wildtype',
      'NEGATIVE_NAME' = 'mutant'
    )
  )
  params %<>% utils::modifyList(new_params)

  params
}

#' Get wildtype wells
#'
#' After a ddPCR plate of type \code{wildtype_mutant_pnpp} has been analyzed,
#' get the wells that were deemed as wildtype.
#' @param plate A ddPCR plate.
#' @return Character vector with well IDs of wildtype wells
#' @seealso
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}\cr
#' \code{\link[ddpcr]{wells_mutant}}
#' @examples
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp) %>% analyze
#' wells_wildtype(plate)
#' }
#' @export
wells_wildtype <- function(plate) {
  stopifnot(plate %>% inherits("wildtype_mutant_pnpp"))
  wells_positive(plate)
}

#' Get mutant wells
#'
#' After a ddPCR plate of type \code{wildtype_mutant_pnpp} has been analyzed,
#' get the wells that were deemed as mutant.
#' @param plate A ddPCR plate.
#' @return Character vector with well IDs of mutant wells
#' @seealso \code{\link[ddpcr]{wildtype_mutant_pnpp}}\cr
#' \code{\link[ddpcr]{wells_wildtype}}
#' @examples
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp) %>% analyze
#' wells_mutant(plate)
#' }
#' @export
wells_mutant <- function(plate) {
  stopifnot(plate %>% inherits("wildtype_mutant_pnpp"))
  wells_negative(plate)
}

#' Plot a ddPCR plate of type wildtype/mutant PNPP
#'
#' Same plot as \code{\link[ddpcr]{plot.pnpp_experiment}} but with a few extra
#' features that are specific to wildtype/mutant PNPP plates. Take a look
#' at \code{\link[ddpcr]{plot.pnpp_experiment}} to see all supported parameters
#' and more information.
#'
#' @inheritParams plot.pnpp_experiment
#' @param col_drops_mutant The colour to use for mutant droplets.
#' @param col_drops_wildtype The colour to use for wildtype droplets.
#' @param col_drops_rain The colour to use for rain droplets.
#' @param show_mutant_freq If \code{TRUE}, show the mutant frequency
#' as a percentage on each well.
#' @param text_size_mutant_freq Text size of the printed mutant frequencies.
#' @param alpha_drops_low_mutant_freq Transparency of mutant droplets
#' in wells with mostly wildtype droplets. In wells where there are very few
#' mutant droplets, it might be useful to make them more visible by increasing
#' their transparency.
#' @param show_low_high_mut_freq Differentiate between wells with a high vs
#' low mutant frequency by having a different background colour to the well.
#' @param bg_mutant The background colour for wells that have a significant
#' mutant cluster.
#' @param bg_wildtype The background colour for wells that have mostly wildtype
#' drops.
#' @param alpha_bg_low_high_mut_freq The transparency value for \code{bg_mutant}
#' and \code{bg_wildtype}.
#' @param ... Parameters to pass to \code{\link[ddpcr]{plot.pnpp_experiment}}.
#' @return A ggplot2 plot object.
#' @seealso
#' \code{\link[ddpcr]{plot.ddpcr_plate}}\cr
#' \code{\link[ddpcr]{plot.pnpp_experiment}}\cr
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}
#' @examples
#' \dontrun{
#' plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp) %>% analyze
#' wells_wildtype(plate)
#' plot(plate)
#' plate <- plate %>% analyze
#' plot(plate)
#' plot(plate, "A01:C05", col_drops_rain = "blue")
#' }
#' @export
plot.wildtype_mutant_pnpp <- function(
  x,
  wells, samples,
  ...,
  col_drops_mutant = "purple3", col_drops_wildtype = "green3",
  col_drops_rain = "black",
  show_mutant_freq = TRUE, text_size_mutant_freq = 4,
  alpha_drops_low_mutant_freq = 0.5,
  show_low_high_mut_freq = TRUE,
  bg_mutant = "purple3", bg_wildtype = "green3",
  alpha_bg_low_high_mut_freq = 0.1
  )
{
  dots <- list(...)
  if (missing(col_drops_mutant) && !is.null(dots[["col_drops_negative"]])) {
    col_drops_mutant <- dots[["col_drops_negative"]]
  }
  if (missing(col_drops_wildtype) && !is.null(dots[["col_drops_positive"]])) {
    col_drops_wildtype <- dots[["col_drops_positive"]]
  }

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
