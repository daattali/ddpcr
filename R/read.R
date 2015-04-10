data_file_regex <- "(.*)_([A-H][0-1][0-9])_Amplitude.csv"

get_name_from_data_files <- function(data_files) {
  data_files_names <- gsub(data_file_regex, "\\1", data_files)
  
  if (data_files_names %>% unique %>% length > 1) {
    warn_msg("not all data files have the same name")
  }  
  
  # in case there are multiple file names, use the most common one
  name <- table(data_files_names) %>% sort(decreasing = TRUE) %>% names %>% .[1]
}

read_dir <- function(plate, dir) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  if (!is_dir(dir)) {
    err_msg(sprintf("could not find directory `%s`", dir))
  }

  # find all the data files
  data_files <- list.files(dir, pattern = data_file_regex)
                           
  name <- get_name_from_data_files(data_files) %>% suppressWarnings
  
  # find all potential metadata files and try to identify the right one
  meta_files <-
    list.files(dir, pattern = ".csv") %>%
    setdiff(data_files)
  ideal_meta_file <- sprintf("%s.csv", name)
  
  if (meta_files %>% length > 1) {
    if (ideal_meta_file %in% meta_files) {
      meta_file <- ideal_meta_file
    } else {
      meta_file <- meta_files[1]
    }
    warn_msg(sprintf("found multiple possible metadata files; using `%s`",
                     meta_file))
  } else {
    meta_file <- meta_files[1]
  }
  
  read_files(data_files, meta_file)
}

read_files <- function(data_files, meta_file) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  tstart <- proc.time()
  

  
  # make sure all given data files are valid paths
  all_files <- 
    vapply(data_files, is_file, FUN.VALUE = logical(1), USE.NAMES = FALSE) %>%
    all
  if (!all_files) {
    err_msg("could not find all files in `data_files`")
  }
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to read data: %s seconds", round(tend-tstart)[1]))
  }
  
  
  
  plate
}


loadData <- function() {
  if (private$debug) {
    tstart <- proc.time()
  }
  
  # Find all the relevant input files
  rawDatasetDir <- file.path("..", "data", self$getName())
  plateFile <- file.path(rawDatasetDir, paste0(self$getName(), ".csv"))
  wellFiles <- list.files(rawDatasetDir, pattern = "_Amplitude.csv", full.names = T)
  
  # Read metadata from plate file
  
  if (!file.exists(plateFile)) {
    stop(sprintf("Could not find plate file in `%s`", normalizePath(plateFile)),
         call. = FALSE)
  }
  plateMetaRaw <- read.csv(plateFile, stringsAsFactors = FALSE)
  colnames(plateMetaRaw) %<>% tolower
  plateColsKeep <- c("well", "sample")
  plateMeta <- plateMetaRaw %>%
    dplyr::select_(~ one_of(plateColsKeep)) %>%
    unique
  plateFileWells <- unique(plateMeta$well)
  
  
  # Read droplet data from all well files
  
  if (length(wellFiles) == 0) {
    stop(sprintf("Could not find any well files in `%s`", normalizePath(rawDatasetDir)),
         call. = FALSE)
  }  

  plateData <-
    lapply(wellFiles, function(x) {
      well <- gsub(".*_([A-H][0-9][0-9])_Amplitude.csv", "\\1", x)
      if (!well %in% plateFileWells) {
        return(NULL)
      }
      wDat <- readr::read_csv(x)
      wDat <- dplyr::select(wDat, select = 1:2)
      wDat$well <- well
      wDat
    })
  plateData %<>% dplyr::bind_rows()

  if (nrow(plateData) == 0) {
    stop(sprintf("Could not find any well files in `%s` that are described in the plate file in `%s`",
                 normalizePath(rawDatasetDir),
                 normalizePath(plateFile)),
         call. = FALSE)
  }

  colnames(plateData) <- c("FAM", "HEX", "well")
  plateData %<>%
    dplyr::select_(~ one_of(c("well", "HEX", "FAM")))

  # Ensure only wells that both have an entry in plate file and have corresponding well file are included
  
  wellFileWells <- unique(plateData$well)
  
  if (length(wellFiles) != length(wellFileWells) | length(plateFileWells) != length(wellFileWells)) {
    warning(paste0("The wells described in the plate file (`", normalizePath(plateFile), "`)",
                   " and the wells found in well files (`", normalizePath(rawDatasetDir), "`)", 
                   " do not match - only wells that appear in both are included."),
            call. = FALSE)
  }  

  plateMeta %<>%
    dplyr::filter_(lazyeval::interp(~ well %in% wellFileWells, well = quote(well)))

  # Initialize all cluster to undefined
  plateData$cluster <- CLUSTER_UNDEFINED
  
  # Populate the metadata with well rows/columns and some initial variables
  plateMeta <- dplyr::left_join(PLATE_INFO, plateMeta, by = "well")
  plateMeta$used <- !is.na(plateMeta$sample)
  plateMeta <-
    plateData %>%
    dplyr::group_by_("well") %>%
    dplyr::summarise_("dropsInitial" = ~ n()) %>%
    dplyr::left_join(plateMeta, ., by = "well") %>%
    dplyr::arrange_(lazyeval::interp(~desc(used), used = quote(used)))
  
  # Set the variables in the plate object
  private$plateData <- plateData
  private$plateMeta <- plateMeta
  private$status <- STATUS_LOADED
  
  if (private$debug) {
    tend <- proc.time()
    message(sprintf("Time to load dataset `%s` (s): %s", self$getName(), round(tend-tstart)[1]))
  }  
  
  invisible(self)
}

Plate$set("public", "loadData", loadData)