## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Plate type: PNPP experiment
#' 
#' PNPP stands for "Positive-Negative;Negative-Positive", which is a reflection
#' of the clusters of non-empty droplets in the wells. Use this plate type when
#' your ddPCR data has three main clusters: double-negative (FAM-HEX-; empty droplets),
#' double-positive (FAM+HEX+; represent the "PP" in PNPP), and singly-positive
#' (either FAM+HEX- or HEX+FAM-; represent the "NP" in PNPP).
#' 
#' Every \code{pnpp_experiment} plate must define which dimension is its \emph{positive
#' dimension}.  The positive dimension is defined as the dimension that corresponds 
#' to the dye that has a high fluoresence intensity in all non-empty droplets. The other
#' dimension is defined as the \emph{variable dimension}. For example, assuming
#' the HEX dye is plotted along the X axis and the FAM dye is along the Y axis,
#' a FAM+/FAM+HEX+ plate will have "Y" as its positive dimension because both
#' non-empty clusters have FAM+ droplets. Similarly, a HEX+/FAM+HEX+ plate will
#' have "X" as its positive dimension.
#' 
#' The positive dimension must be set in order to use a \code{pnpp_experiment}.
#' It is not recommended to use this type directly; instead you should use one
#' of the subtypes. If you do use this type directly, you must set the positive
#' dimension with \code{\link[ddpcr]{positive_dim}}.
#' 
#' \href{https://github.com/daattali/ddpcr#extend}{See the README} for
#' more information on plate types.
#' 
#' @seealso
#' \code{\link[ddpcr]{plate_types}}
#' \code{\link[ddpcr]{fam_positive_pnpp}}
#' \code{\link[ddpcr]{hex_positive_pnpp}}
#' \code{\link[ddpcr]{wildtype_mutant_pnpp}}
#' \code{\link[ddpcr]{positive_dim}}
#' \code{\link[ddpcr]{wells_positive}}
#' \code{\link[ddpcr]{wells_negative}}
#' @name pnpp_experiment
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$pnpp_experiment)
#' type(plate)
#' } 
NULL

plate_types[['pnpp_experiment']] <- "pnpp_experiment"

#' Define plate type parameters for PNPP experiments
#' @inheritParams define_params
define_params.pnpp_experiment <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME'      = 'positive',
      'NEGATIVE_NAME'      = 'negative',
      'POSITIVE_DIMENSION' = NA   # Must be set using positive_dim() function
    ),
    'CLASSIFY' = list(
      'CLUSTERS_BORDERS_NUM_SD'      = 3,
      'ADJUST_BW_MIN'                = 4,
      'ADJUST_BW_MAX'                = 20,
      # a well with a mutant frequency that is statistically significantly above
      # this value is considered a mutant well
      'SIGNIFICANT_NEGATIVE_FREQ'    = 0.01,
      # the p value to use for the statistical significance test used to determine
      # is a well is mutant or not
      'SIGNIFICANT_P_VALUE'          = 0.01
    ),
    'RECLASSIFY' = list(
      'MIN_WELLS_NEGATIVE_CLUSTER'   = 4,
      'BORDER_RATIO_QUANTILE'        = 0.75
    )
  )
  params %<>% modifyList(new_params)
  
  params
}

#' Define droplet clusters for PNPP experiments
#' @inheritParams define_clusters
define_clusters.pnpp_experiment <- function(plate) {
  clusters <- NextMethod("define_clusters")
  
  c(clusters,
    'RAIN',
    'POSITIVE',
    'NEGATIVE'
  )
}

#' Define analysis steps for PNPP experiments
#' @inheritParams define_steps
define_steps.pnpp_experiment <- function(plate) {
  steps <- NextMethod("define_steps")
  
  c(steps,
    list(
      'CLASSIFY' = 'classify_droplets',
      'RECLASSIFY' = 'reclassify_droplets'
    ))
}

#' Positive dimension in a PNPP experiment
#' 
#' Get or set the positive dimension (X or Y), which is defined as the dimension
#' that has a high fluorescence intensity in all non-empty drops in a
#' \code{pnpp_experiment} plate.
#' @param plate A ddPCR plate.
#' @param value The dimension to set as the positive dimension ("X" or "Y")
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{variable_dim}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = plate_types$pnpp_experiment)
#' positive_dim(plate) <- "Y"
#' @name positive_dim
NULL

#' @rdname positive_dim
#' @export
#' @keywords internal
positive_dim <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% toupper
}
#' @rdname positive_dim
#' @export
#' @keywords internal
`positive_dim<-` <- function(plate, value = c("X", "Y")) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  value <- match.arg(value)
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value
  plate
}

#' Variable dimension in a PNPP experiment
#' 
#' Get or set the variable dimension (X or Y), which is defined as the dimension
#' that can have both high and low fluorescence intensities in the non-empty
#' drops in a \code{pnpp_experiment} plate.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{positive_dim}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = plate_types$pnpp_experiment)
#' variable_dim(plate) <- "Y"
#' variable_dim(plate)
#' positive_dim(plate)
#' @keywords internal
#' @name variable_dim
NULL

#' @rdname variable_dim
#' @export
#' @keywords internal
variable_dim <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% toupper %>% other_dim
}
#' @rdname variable_dim
#' @export
#' @keywords internal
`variable_dim<-` <- function(plate, value = c("X", "Y")) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  value <- match.arg(value)
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value %>% other_dim
  plate
}

#' Given an axis (X or Y), return the other
#' @examples 
#' other_dim("X")
#' other_dim("Y")
#' @export
#' @keywords internal
other_dim <- function(dim = c("X", "Y")) {
  dim <- match.arg(dim)
  if(toupper(dim) == "X") "Y" else "X"
}

#' Name of dye in positive dimension in PNPP experiment
#' 
#' Get the name of the dye that is along the positive dimension.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{positive_dim}}
#' @export
#' @keywords internal
positive_dim_var <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  plate %>%
    positive_dim %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}
}

#' Name of dye in variable dimension in PNPP experiment
#' 
#' Get the name of the dye that is along the variable dimension.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{variable_dim}}
#' @keywords internal
#' @export
variable_dim_var <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  plate %>%
    variable_dim %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}  
}

#' Get positive wells
#' 
#' After a ddPCR plate of type \code{pnpp_experiment} has been analyzed,
#' get the wells that were deemed as having mostly positive droplets.
#' @param plate A ddPCR plate.
#' @return Character vector with well IDs of positive wells
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}}
#' \code{\link[ddpcr]{wells_negative}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$pnpp_experiment) %>% analyze
#' wells_positive(plate)
#' }
#' @export
wells_positive <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  
  if (status(plate) < step(plate, 'CLASSIFY')) {
    return()
  }
  
  plate %>%
    plate_meta %>%
    dplyr::filter_(lazyeval::interp(
      ~ !var,
      var = as.name(meta_var_name(plate, "significant_negative_cluster"))
    )) %>%
    .[['well']]
}

#' Get negative wells
#' 
#' After a ddPCR plate of type \code{pnpp_experiment} has been analyzed,
#' get the wells that were not deemed as having mostly positive droplets.
#' @param plate A ddPCR plate.
#' @return Character vector with well IDs of negative wells
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}}
#' \code{\link[ddpcr]{wells_positive}}
#' @examples 
#' \dontrun{
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir, type = plate_types$pnpp_experiment) %>% analyze
#' wells_negative(plate)
#' }
#' @export
wells_negative <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  
  if (status(plate) < step(plate, 'CLASSIFY')) {
    return(c())
  }  
  
  plate %>%
    plate_meta %>%
    dplyr::filter_(as.name(meta_var_name(plate, "significant_negative_cluster"))) %>%
    .[['well']]
}

#' Name of variable in PNPP experiment metadata
#' 
#' A default PNPP experiment uses the names "positive" and "negative" for its two
#' non-empty clusters. If they are changed (for example, to "wildtype" and "mutant"),
#' then any variable in the metadata will be renamed to use these names.
#' \code{meta_var_name} translates a default metadata variable name to the correct one.
#' @seealso \code{\link[ddpcr]{pnpp_experiment}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = plate_types$pnpp_experiment)
#' params(plate, 'GENERAL', 'NEGATIVE_NAME') <- "mutant"
#' meta_var_name(plate, 'num_negative_drops')
#' @keywords internal
#' @export
meta_var_name <- function(plate, var) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  var %>%
    gsub("negative", params(plate, 'GENERAL', 'NEGATIVE_NAME'), .) %>%
    gsub("positive", params(plate, 'GENERAL', 'POSITIVE_NAME'), .)
}