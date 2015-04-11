DATA_FILE_REGEX <- "^(.*)_([A-H][0-1][0-9])_Amplitude.csv$"
DATA_FILE_REGEX_NAME <- "\\1"
DATA_FILE_REGEX_WELL <- "\\2"

get_name_from_data_file <- function(data_file) {
  gsub(DATA_FILE_REGEX, DATA_FILE_REGEX_NAME, data_file)
}

get_well_from_data_file <- function(data_file) {
  gsub(DATA_FILE_REGEX, DATA_FILE_REGEX_WELL, data_file)
}

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
}

read_dir <- function(plate, dir) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!is_dir(dir)) {
    err_msg(sprintf("could not find directory `%s`", dir))
  }

  # find all the data files
  data_files <- list.files(dir, pattern = DATA_FILE_REGEX, full.names = TRUE)
                           
  name <- get_consensus_name_from_data_files(data_files) %>% suppressWarnings
  
  # find all potential metadata files and try to identify the right one
  meta_files <-
    list.files(dir, pattern = ".csv$", full.names = TRUE) %>%
    setdiff(data_files)
  ideal_meta_file <- file.path(dir, sprintf("%s.csv", name))
  
  if (meta_files %>% length > 1) {
    if (ideal_meta_file %in% meta_files) {
      meta_file <- ideal_meta_file
    } else {
      meta_file <- meta_files[1]
    }
    warn_msg(sprintf("found multiple possible metadata files; using `%s`",
                     basename(metafile)))
  } else {
    meta_file <- meta_files[1]
  }
  
  read_files(plate, data_files, meta_file)
}

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
  } else if (!is_file(meta_file)) {
    err_msg("could not find metadata file")
  }
  
  tstart <- proc.time()
  
  # read the droplets data
  plate_data <-
    lapply(data_files, function(x) {
      wellNum <- get_well_from_data_file(x)
      wdat <-
        readr::read_csv(x) %>%
        dplyr::select_(~ 1:2) %>%
        dplyr::mutate_(.dots = setNames(list(~ wellNum), "well"))
      wdat
    })
  plate_data %<>%
    dplyr::bind_rows() %>%
    magrittr::set_colnames(c("FAM", "HEX", "well")) %>%
    dplyr::select_(~ one_of(c("well", "HEX", "FAM")))   %>%
    dplyr::mutate_(.dots = setNames(list(~ CLUSTER_UNDEFINED), "cluster"))
  plate_data(plate) <- plate_data
  
  
  dplyr::left_join(d,m,by="well") %>% dplyr::mutate(sample.x = ifelse(is.na(sample.y), sample.x, sample.y)) %>% dplyr::rename_(.dots = setNames(list("sample.x"), "sample")) %>% dplyr::select_(~ -sample.y)
  
  
  # read the meta data (we only care about the well -> sample mapping)
  if (!is.null(meta_file)) {
    meta_cols_keep <- c("well", "sample")
    plate_meta_raw <-
      read.csv(meta_file, stringsAsFactors = FALSE)  %>%
      s
    colnames(plateMetaRaw) %<>% tolower
    plateColsKeep <- c("well", "sample")
    plateMeta <- plateMetaRaw %>%
      dplyr::select_(~ one_of(plateColsKeep)) %>%
      unique
    
  }

  
  
  
  
  
  # set the plate's name based on the file paths
  name(plate) <- get_consensus_name_from_data_files(data_files)
  
  status(plate) <- STATUS_LOADED
  
  tend <- proc.time()
  message(sprintf("Time to read data: %s seconds", round(tend-tstart)[1]))
    
  plate
}


#   }  
# 
#   plateMeta %<>%
#     dplyr::filter_(lazyeval::interp(~ well %in% wellFileWells, well = quote(well)))
# 
#   
#   # Populate the metadata with well rows/columns and some initial variables
#   plateMeta <- dplyr::left_join(PLATE_INFO, plateMeta, by = "well")
#   plateMeta$used <- !is.na(plateMeta$sample)
#   plateMeta <-
#     plateData %>%
#     dplyr::group_by_("well") %>%
#     dplyr::summarise_("dropsInitial" = ~ n()) %>%
#     dplyr::left_join(plateMeta, ., by = "well") %>%
#     dplyr::arrange_(lazyeval::interp(~desc(used), used = quote(used)))
#   
#   # Set the variables in the plate object
#   private$plateData <- plateData
#   private$plateMeta <- plateMeta
#   private$status <- STATUS_LOADED
#   