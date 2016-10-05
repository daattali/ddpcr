## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains functions related to reading in data. None of them are exported
# as these functions should only be used by the original package.

# Read a plate data from a directory or list of files
read_plate <- function(plate, dir, data_files, meta_file) {
  if (!missing(dir)) {
    read_dir(plate, dir)
  } else if (!missing(data_files)) {
    read_files(plate, data_files, meta_file)
  } else {
    err_msg("either `dir` or `data_files` must be specified")
  }
}

# Read a plate data from a given directory
read_dir <- function(plate, dir) {
  stopifnot(plate %>% inherits("ddpcr_plate"))

  if (!is_dir(dir)) {
    err_msg(sprintf("could not find directory `%s`", dir))
  }
  
  # find the data files in the directory and use them to read the plate
  data_files <- find_data_files(dir)
  if (length(data_files) == 0) {
    err_msg("Could not find any valid data files (ddpcr expects the \"_Amplitude\" files exported by QuantaSoft)")
  }
  name <- suppressWarnings(get_consensus_name_from_data_files(data_files))
  meta_file <- find_meta_file(dir, name)
  
  read_files(plate, data_files, meta_file)
}

# Read a plate data from a given list of files
read_files <- function(plate, data_files, meta_file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  # make sure all given data files are valid data files
  if (missing(data_files) || length(data_files) == 0) {
    err_msg("no data files provided")
  }
  all_files <- 
    vapply(data_files, is_file, FUN.VALUE = logical(1), USE.NAMES = FALSE) %>%
    all
  if (!all_files) {
    err_msg("could not find all data files")
  }
  if (!all(grepl(DATA_FILE_REGEX, data_files))) {
    err_msg(paste("not all data files provided are valid data files",
                  "(ddpcr expects only the \"_Amplitude\" files exported by QuantaSoft)"))
  }
  
  # make sure metadata file is a valid path
  if (missing(meta_file)) {
    meta_file <- NULL
    warn_msg("no metadata file provided")
  } else if (!is.null(meta_file) && !is_file(meta_file)) {
    err_msg("could not find metadata file")
  }
  
  step_begin("Reading data files into plate")
  
  # read the droplets data
  # I purposely keep the wells as character rather than factor because
  # the data.frame is large and it's much faster to search through it using
  # dplyr::filter when using character
  tryCatch({
    plate_data <-
      lapply(data_files, function(x) {
        wellNum <- get_well_from_data_file(x)
        wdat <- suppressMessages(readr::read_csv(x, progress = FALSE))
        wdat <- dplyr::select_(wdat, ~ 2:1)
        wdat[['well']] <- wellNum
        wdat
      }) %>%
      dplyr::bind_rows() %>%
      move_front("well") %>%
      dplyr::arrange_("well")
  },
  error = function(err) {
    err_msg("there was a problem reading one or more of the data files")
  })
  
  plate_data(plate) <- plate_data
  
  # Read the metadata file if one was given
  if (!is.null(meta_file)) {
    tryCatch({
      plate_meta <-
        utils::read.csv(meta_file, stringsAsFactors = FALSE) %>%
        magrittr::set_colnames(colnames(.) %>% tolower)
    },
    error = function(err) {
      err_msg("there was a problem with the metadata file")
    })
    plate_meta(plate) <- plate_meta
  }
  
  # set the plate's name based on the file paths
  name(plate) <- get_consensus_name_from_data_files(data_files)
  
  step_end()
  
  plate
}

# -------- helper functions

# regex for droplet data file
DATA_FILE_REGEX <- "^(.*)_([A-H][0-1][0-9])_Amplitude.csv$"
DATA_FILE_REGEX_NAME <- "\\1"
DATA_FILE_REGEX_WELL <- "\\2"

# extract the name of a file from a filename
get_name_from_data_file <- function(data_file) {
  gsub(DATA_FILE_REGEX, DATA_FILE_REGEX_NAME, data_file)
}

# extract the well ID from a filename
get_well_from_data_file <- function(data_file) {
  gsub(DATA_FILE_REGEX, DATA_FILE_REGEX_WELL, data_file)
}

# extract the most common filename from a list of files
get_consensus_name_from_data_files <- function(data_files) {
  data_files_names <- get_name_from_data_file(data_files)
  
  if (data_files_names %>% unique %>% length > 1) {
    warn_msg("not all data files have the same name")
  }  
  
  # in case there are multiple file names, use the most common one
  name <-
    table(data_files_names) %>%
    sort(decreasing = TRUE) %>%
    names %>%
    .[1] %>%
    basename
  
  name
}

# find the data files in a directory
find_data_files <- function(dir) {
  list.files(dir, pattern = DATA_FILE_REGEX, full.names = TRUE)
}

# find the metadata file in a directory, or return NULL if it doesn't exist
find_meta_file <- function(dir, name) {
  meta_file <- file.path(dir, sprintf("%s.csv", name))
  if (is_file(meta_file)) {
    return(meta_file)
  } else {
    warn_msg(sprintf("could not find metadata file; looked for `%s`", meta_file))
    return(NULL)
  }
}
