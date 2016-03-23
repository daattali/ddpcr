## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains helper functions related to a ddPCR plate creation. None
# of them are exported as they should only be used by the original package.

# Return an empty plate
empty_plate <- function() {
  list(
    plate_data = NULL,
    plate_meta = NULL,
    name       = NULL,
    status     = NULL,
    params     = NULL,
    clusters   = NULL,
    steps      = NULL,
    dirty      = NULL,
    version    = NULL
  )
}

# Create and set up a brand new plate with a specific type
setup_new_plate <- function(type) {
  plate <- empty_plate()
  setup_plate(plate, type)
}

# Given a plate object, set it up as a plate with a given type
setup_plate <- function(plate, type) {
  plate <- set_plate_type(plate, type)
  plate <- set_default_params(plate)
  plate
}

# Given a plate that already had a type, initialize it
init_plate <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  step_begin(sprintf("Initializing plate of type `%s`", type(plate)))
  
  plate %<>%
    set_default_clusters %>%
    set_default_steps %>%
    init_data %>%
    init_meta
  
  status(plate) <- step(plate, 'INITIALIZE')
  plate[['version']] <- as.character(utils::packageVersion("ddpcr"))
  plate[['dirty']] <- FALSE
  step_end()
  
  plate
}

# Initialize the droplet data of a plate
init_data <- function(plate) {
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  
  new_plate_data <- plate_data(plate)
  colnames(new_plate_data) <- c("well", x_var, y_var)
  new_plate_data <- dplyr::select_(new_plate_data, "well", x_var, y_var)
  new_plate_data[['cluster']] <- cluster(plate, 'UNDEFINED')
  new_plate_data[[x_var]] <- as.integer(new_plate_data[[x_var]])
  new_plate_data[[y_var]] <- as.integer(new_plate_data[[y_var]])
  new_plate_data <- dplyr::arrange_(new_plate_data, ~ well)
  plate_data(plate) <- new_plate_data
  
  plate
}

# Initialize the metadata of a plte
init_meta <- function(plate) {
  plate_data <- plate_data(plate)
  plate_meta <- plate_meta(plate)
  
  # read the meta data file (we only care about the well -> sample mapping)
  if (is.null(plate_meta)) {
    plate_meta <- DEFAULT_PLATE_META
  } else {
    meta_cols_keep <- c("well", "sample")
    tryCatch({
      plate_meta %<>%
        dplyr::select_(~ one_of(meta_cols_keep)) %>%
        unique %>%
        merge_dfs_overwrite_col(DEFAULT_PLATE_META, ., "sample", "well")
    },
    error = function(err) {
      err_msg("the metadata file is invalid")
    })
  }
  
  # populate the metadata with some initial variables
  wells_used <- plate_data[['well']] %>% unique
  plate_meta[['used']] <- plate_meta[['well']] %in% wells_used
  plate_meta[['sample']][!plate_meta[['used']]] <- NA
  plate_meta <-
    plate_data %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops" = ~ n()) %>%
    dplyr::left_join(plate_meta, ., by = "well") %>%
    arrange_meta
  
  plate_meta(plate) <- plate_meta
  plate
}

# Set the type of a plate. Supports multi-level inheritance.
# All types are by default ddpcr_plate, but users can define new plate types
# that inherit from another one (which means they will use the same params/
# methods)
set_plate_type <- function(plate, type) {
  if (!missing(type) && is.null(type) && class(plate) != "list") {
    return(plate)
  }
  
  if (missing(type) || is.null(type) || !nzchar(type)) {
    type <- NULL
  }  
  
  # Add the given type to the classlist (initially the class will be "list"
  # because that's the mode of a plate - we want to exclude that one)
  new_class <- type
  if (class(plate)[1] != "list") {
    new_class <- c(class(plate), new_class)
  }
  class(plate) <- new_class
  
  # Recursively add the plate type of the parent
  set_plate_type(plate,
                 structure(plate, class = type) %>% parent_plate_type)
}

# Set the clusters of a plate to the default clusters for the plate type
set_default_clusters <- function(plate) {
  clusters(plate) <- define_clusters(plate)
  plate
}

# Set the steps of a plate to the default steps for the plate type
set_default_steps <- function(plate) {
  steps(plate) <- define_steps(plate)
  plate
}