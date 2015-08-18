## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' PNPP experiment
#' 
#' "Positive-Positive;Negative-Positive assay" type. Use this assay when
#' your ddPCR data has 3 main clusters: one cluster near the bottom left that
#' corresponds to empty drops, one cluster near the top right with drops that
#' have a high intensity in both x and y dimensions (Positive-Positive), and one
#' cluster at one of the other two corners with drops that have a high intensity
#' in one diemnsion but a low intensity in the other (Negative-Positive).
#' 
#' TODO what this assay looks like (POSITIVE_DIMENSION must be set, either
#' in params or by defining a child type)
#' 
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' new_plate(dir = dir, type = plate_types$pnpp_experiment)
#' new_plate(dir = dir, type = plate_types$fam_positive_pnpp)
#' @seealso
#' \code{\link[ddpcr]{new_plate}},
#' \code{\link[ddpcr]{positive_dim}},
#' \code{\link[ddpcr]{wells_positive}},
#' \code{\link[ddpcr]{wells_negative}}
#' @export
PNPP_EXPERIMENT <- "pnpp_experiment"

#' @export
define_clusters.pnpp_experiment <- function(plate) {
  clusters <- NextMethod("define_clusters")
  
  clusters %>%
    add_clusters(c(
      'RAIN',
      'POSITIVE',
      'NEGATIVE'
    ))
}

#' @export
define_steps.pnpp_experiment <- function(plate) {
  steps <- NextMethod("define_steps")
  
  steps %>%
    add_steps(list(
      'CLASSIFY' = 'classify_droplets',
      'RECLASSIFY' = 'reclassify_droplets'
    ))
}

#' @export
define_params.pnpp_experiment <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME'      = 'positive',
      'NEGATIVE_NAME'      = 'negative',
      'POSITIVE_DIMENSION' = NA   # Must be set by the child
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

#' Positive dimension in a PNPP experiment
#' 
#' Get or set the dimension (X or Y) that has a high intensity in all non-empty
#' drops in a \code{PNPP_EXPERIMENT}.
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
`positive_dim<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  value %<>% toupper
  stopifnot(value == "X" || value == "Y")
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value
  plate
}

#' Variable dimension in a PNPP experiment
#' 
#' Get or set the dimension (X or Y) that can have both high and low intensities
#' in the non-empty drops in a \code{plate_types$pnpp_experiment}.
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
#' @keywords internal
#' @export
variable_dim <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% toupper %>% other_dim
}
#' @rdname variable_dim
#' @keywords internal
#' @export
`variable_dim<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  value %<>% toupper
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value %>% other_dim
  plate
}

#' Name of variable of positive dimension in PNPP experiment
#' 
#' Get the name of the variable that is along the dimension where all filled 
#' drops should be positive.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{positive_dim}}
#' @keywords internal
#' @export
positive_dim_var <- function(plate) {
  stopifnot(plate %>% inherits("pnpp_experiment"))
  plate %>%
    positive_dim %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}
}

#' Name of variable of variable dimension in PNPP experiment
#' 
#' Get the name of the variable that is along the dimension where the droplets
#' will cluster into two groups.
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

#' Given an axis (X or Y), return the other
#' @examples 
#' other_dim("X")
#' other_dim("y")
#' @keywords internal
#' @export
other_dim <- function(dim) {
  ifelse(dim %>% toupper == "X", "Y", "X")
}

#' Name of variable in PNPP experiment metadata
#' 
#' A default PNPP experiment uses the names "positive" and "negative" for its two
#' non-empty clusters. If they are changed, then any variable in the metadata
#' will be renamed to use these names. \code{meta_var_name} translated a
#' default metadata variable name to the correct one.
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

#' Get wells containing mostly positive droplets in a PNPP experiment
#' 
#' In a PNPP experiment, the two main non-empty clusters of drops are the negative
#' and positive clusters. The positive cluster appears in every well, while the
#' negative cluster is not necessarily in every well. \code{wells_positive}
#' returns a list of wells that have mostly positive drops.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{wells_negative}}
#' @param x A ddPCR plate of type pnpp_experiment
#' @return Vector of wells with mostly positive drops.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% wells_positive
#' @export
wells_positive <- function(x) {
  stopifnot(x %>% inherits("pnpp_experiment"))
  
  if (status(x) < step(x, 'CLASSIFY')) {
    return(c())
  }
  
  x %>%
    plate_meta %>%
    dplyr::filter_(lazyeval::interp(
      ~ !var,
      var = as.name(meta_var_name(x, "significant_negative_cluster"))
    )) %>%
    .[['well']]
}

#' Get wells containing many negative droplets in a PNPP experiment
#' 
#' In a PNPP experiment, the two main non-empty clusters of drops are the negative
#' and positive clusters. The positive cluster appears in every well, while the
#' negative cluster is not necessarily in every well. \code{wells_negative}
#' returns a list of wells that have a significant number of negative drops.
#' @seealso
#' \code{\link[ddpcr]{pnpp_experiment}},
#' \code{\link[ddpcr]{wells_positive}}
#' @param x A ddPCR plate of type pnpp_experiment
#' @return Vector of wells with significant number of negative drops.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% wells_negative
#' @export
wells_negative <- function(x) {
  stopifnot(x %>% inherits("pnpp_experiment"))
  
  if (status(x) < step(x, 'CLASSIFY')) {
    return(c())
  }  
  
  x %>%
    plate_meta %>%
    dplyr::filter_(as.name(meta_var_name(x, "significant_negative_cluster"))) %>%
    .[['well']]
}
