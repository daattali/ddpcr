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