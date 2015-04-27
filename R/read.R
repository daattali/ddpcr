DATA_FILE_REGEX <- "^(.*)_([A-H][0-1][0-9])_Amplitude.csv$"
DATA_FILE_REGEX_NAME <- "\\1"
DATA_FILE_REGEX_WELL <- "\\2"

# extract the name of a file from a filename
get_name_from_data_file <- function(data_file) {
  gsub(DATA_FILE_REGEX, DATA_FILE_REGEX_NAME, data_file)
}

# extract the well from a filename
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

# read a plate data from a given directory
read_dir <- function(plate, dir) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!is_dir(dir)) {
    err_msg(sprintf("could not find directory `%s`", dir))
  }
  
  # find the data files in the directory and use them to read the plate
  data_files <- find_data_files(dir)
  name <- suppressWarnings(get_consensus_name_from_data_files(data_files))
  meta_file <- find_meta_file(dir, name)
  
  read_files(plate, data_files, meta_file)
}

# read a plate data from a given list of files
read_files <- function(plate, data_files, meta_file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  # make sure all given data files are valid paths
  if (missing(data_files) || length(data_files) == 0) {
    err_msg("no data files provided")
  }
  all_files <- 
    vapply(data_files, is_file, FUN.VALUE = logical(1), USE.NAMES = FALSE) %>%
    all
  if (!all_files) {
    err_msg("could not find all data files")
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
  x_var <- x_var(plate)
  y_var <- y_var(plate)
  plate_data <-
    lapply(data_files, function(x) {
      wellNum <- get_well_from_data_file(x)
      wdat <-
        readr::read_csv(x) %>%
        dplyr::select_(~ 1:2) %>%
        dplyr::mutate_(.dots = setNames(list(~ wellNum), "well"))
      wdat
    }) %>%
    dplyr::bind_rows() %>%
    magrittr::set_colnames(c(y_var, x_var, "well")) %>%
    dplyr::select_("well", x_var, y_var) %>%  # reorder so that well is first
    dplyr::mutate_(.dots = setNames(
      list(~ CLUSTER_UNDEFINED,
           lazyeval::interp(~ as.integer(var), var = as.name(x_var)),
           lazyeval::interp(~ as.integer(var), var = as.name(y_var))
      ),
      c("cluster", x_var, y_var))) %>%
    dplyr::arrange_(~ well)  # arrange by wells alphabetically
  
  # start with a default metadata
  plate_meta <- DEFAULT_PLATE_META
  
   # read the meta data file (we only care about the well -> sample mapping)
  if (!is.null(meta_file)) {
    meta_cols_keep <- c("well", "sample")
    plate_meta_samples <-
      read.csv(meta_file, stringsAsFactors = FALSE)  %>%
      magrittr::set_colnames(colnames(.) %>% tolower) %>%
      dplyr::select_(~ one_of(meta_cols_keep)) %>%
      unique
    
    plate_meta <-
      merge_dfs_overwrite_col(plate_meta, plate_meta_samples, "sample", "well")
  } 

  # populate the metadata with some initial variables
  wells_used <- plate_data[['well']] %>% unique
  plate_meta[['used']] <- plate_meta[['well']] %in% wells_used
  plate_meta <-
    plate_data %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("drops" = ~ n()) %>%
    dplyr::left_join(plate_meta, ., by = "well") %>%
    arrange_meta
  
  # set the plate's name based on the file paths
  name(plate) <- get_consensus_name_from_data_files(data_files)
  
  # save the data and metadata and update the plate status
  plate_data(plate) <- plate_data
  plate_meta(plate) <- plate_meta
  status(plate) <- STATUS_INIT
  
  step_end()
  
  plate
}

# read a plate data from a directory or list of files
read_plate <- function(plate, dir, data_files, meta_file) {
  if (!missing(dir)) {
    read_dir(plate, dir)
  } else if (!missing(data_files)) {
    read_files(plate, data_files, meta_file)
  } else {
    err_msg("either `dir` or `data_files` must be specified")
  }
}