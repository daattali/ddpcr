## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

#' PPNP assay
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
#' new_plate(dir = dir, type = PPNP_ASSAY)
#' new_plate(dir = dir, type = FAM_POSITIVE_PPNP)
#' @seealso
#' \code{\link[ddpcr]{new_plate}},
#' \code{\link[ddpcr]{positive_dim}},
#' \code{\link[ddpcr]{wells_positive}},
#' \code{\link[ddpcr]{wells_negative}}
#' @export
PPNP_ASSAY <- "ppnp_assay"

#' @export
define_clusters.ppnp_assay <- function(plate) {
  clusters <- NextMethod("define_clusters")
  
  clusters %>%
    add_clusters(c(
      'RAIN',
      'POSITIVE',
      'NEGATIVE'
    ))
}

#' @export
define_steps.ppnp_assay <- function(plate) {
  steps <- NextMethod("define_steps")
  
  steps %>%
    add_steps(list(
      'CLASSIFY' = 'classify_droplets',
      'RECLASSIFY' = 'reclassify_droplets'
    ))
}

#' @export
define_params.ppnp_assay <- function(plate) {
  params <- NextMethod("define_params")
  
  new_params <- list(
    'GENERAL' = list(
      'POSITIVE_NAME'      = 'positive',
      'NEGATIVE_NAME'      = 'negative',
      'POSITIVE_DIMENSION' = NA   # Must be set by the child
    ),
    'REMOVE_FAILURES' = list(
      'FAST'   = TRUE
    ),
    'CLASSIFY' = list(
      'NUM_ATTEMPTS_SEGREGATE'       = 1,
      'SEGREGATE_RATIO_THRESHOLD'    = 0.75,
      'CLUSTERS_BORDERS_NUM_SD'      = 3,
      'NO_NEG_CLUSTER_BORDER_NUM_SD' = 10,
      'ADJUST_MIN'                   = 4,
      'ADJUST_MAX'                   = 20,
      'METHOD'                       = 'density_inflection_points'
    ),
    'RECLASSIFY' = list(
      'MIN_WELLS_NEGATIVE_CLUSTER'   = 4,
      'BORDER_RATIO_QUANTILE'        = 0.75
    )
  )
  params %<>% modifyList(new_params)
  
  params
}

#' Positive dimension in a PPNP assay
#' 
#' Get or set the dimension (X or Y) that has a high intensity in all non-empty
#' drops in a \code{PPNP_ASSAY}.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{variable_dim}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = PPNP_ASSAY)
#' positive_dim(plate) <- "Y"
#' @name positive_dim
NULL

#' @rdname positive_dim
#' @export
positive_dim <- function(plate) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% toupper
}
#' @rdname positive_dim
#' @export
`positive_dim<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  value %<>% toupper
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value
  plate
}

#' Variable dimension in a PPNP assay
#' 
#' Get or set the dimension (X or Y) that can have both high and low intensities
#' in the non-empty drops in a \code{PPNP_ASSAY}.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{positive_dim}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = PPNP_ASSAY)
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
  stopifnot(plate %>% inherits("ppnp_assay"))
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') %>% toupper %>% other_dim
}
#' @rdname variable_dim
#' @keywords internal
#' @export
`variable_dim<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  value %<>% toupper
  params(plate, 'GENERAL', 'POSITIVE_DIMENSION') <- value %>% other_dim
  plate
}

#' Name of variable of positive dimension in PPNP assay
#' 
#' Get the name of the variable that is along the dimension where all filled 
#' drops should be positive.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{positive_dim}}
#' @keywords internal
#' @export
positive_dim_var <- function(plate) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  plate %>%
    positive_dim %>%
    {params(plate, 'GENERAL', sprintf('%s_VAR', .))}
}

#' Name of variable of variable dimension in PPNP assay
#' 
#' Get the name of the variable that is along the dimension where the droplets
#' will cluster into two groups.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{variable_dim}}
#' @keywords internal
#' @export
variable_dim_var <- function(plate) {
  stopifnot(plate %>% inherits("ppnp_assay"))
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

#' Name of variable in PPNP assay metadata
#' 
#' A default PPNP assay uses the names "positive" and "negative" for its two
#' non-empty clusters. If they are changed, then any variable in the metadata
#' will be renamed to use these names. \code{meta_var_name} translated a
#' default metadata variable name to the correct one.
#' @seealso \code{\link[ddpcr]{PPNP_ASSAy}}
#' @examples 
#' dir <- system.file("sample_data", "small", package = "ddpcr")
#' plate <- new_plate(dir = dir, type = PPNP_ASSAY)
#' params(plate, 'GENERAL', 'NEGATIVE_NAME') <- "mutant"
#' meta_var_name(plate, 'num_negative_drops')
#' @keywords internal
#' @export
meta_var_name <- function(plate, var) {
  stopifnot(plate %>% inherits("ppnp_assay"))
  var %>%
    gsub("negative", params(plate, 'GENERAL', 'NEGATIVE_NAME'), .) %>%
    gsub("positive", params(plate, 'GENERAL', 'POSITIVE_NAME'), .)
}

#' Get wells containing mostly positive droplets in a PPNP assay
#' 
#' In a PPNP assay, the two main non-empty clusters of drops are the negative
#' and positive clusters. The positive cluster appears in every well, while the
#' negative cluster is not necessarily in every well. \code{wells_positive}
#' returns a list of wells that have mostly positive drops.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{wells_negative}}
#' @param x A ddPCR plate of type PPNP_ASSAY
#' @return Vector of wells with mostly positive drops.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% wells_positive
#' @export
wells_positive <- function(x) {
  stopifnot(x %>% inherits("ppnp_assay"))
  
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

#' Get wells containing many negative droplets in a PPNP assay
#' 
#' In a PPNP assay, the two main non-empty clusters of drops are the negative
#' and positive clusters. The positive cluster appears in every well, while the
#' negative cluster is not necessarily in every well. \code{wells_negative}
#' returns a list of wells that have a significant number of negative drops.
#' @seealso
#' \code{\link[ddpcr]{PPNP_ASSAy}},
#' \code{\link[ddpcr]{wells_positive}}
#' @param x A ddPCR plate of type PPNP_ASSAY
#' @return Vector of wells with significant number of negative drops.
#' @examples 
#' file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
#' plate <- load_plate(file)
#' plate %>% wells_negative
#' @export
wells_negative <- function(x) {
  stopifnot(x %>% inherits("ppnp_assay"))
  
  if (status(x) < step(x, 'CLASSIFY')) {
    return(c())
  }  
  
  x %>%
    plate_meta %>%
    dplyr::filter_(as.name(meta_var_name(x, "significant_negative_cluster"))) %>%
    .[['well']]
}
