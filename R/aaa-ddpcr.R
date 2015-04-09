#' ddPCR Plate
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
Plate <- R6::R6Class(
  classname = "ddpcr_plate",
  lock = FALSE,
  class = TRUE,
  portable = TRUE,
  
  public = list(
    initialize = function(dataset = "2-26-2014-BRAFWTNEGASSAY-FFPEDNA-CRC-1-41", debug = TRUE,
                          m, d, p) {
      private$status <- STATUS_UNDEFINED
      
      private$debug <- debug
      private$datasetName <- dataset
      
      private$status <- STATUS_INIT
      
      if (!missing(m)) {
        self$setm(m)
      }
      if (!missing(d)) {
        self$setd(d)
      }
      if (!missing(p)) {
        self$setm(p$getMeta())
        self$setd(p$getData())
        private$datasetName <- p$getName()
        private$status <- p$getStatus()
      }
    },
    
    getData = function() {
      private$plateData
    },
    
    getMeta = function(onlyUsed = FALSE) {
      if (onlyUsed) {
        dplyr::filter_(private$plateMeta, lazyeval::interp(~ used, used = quote(used)))
      } else {
        private$plateMeta
      }
    },
    
    getName = function() {
      private$datasetName
    },
    
    getStatus = function() {
      private$status
    },
    
    analyze = function() {
      self$loadData()         # step 0 - load data
      self$removeOutliers()   # step 1 - remove outlier droplets
      self$removeFailures()   # step 2 - remove failed wells
      self$markEmpty()        # step 3 - remove empty droplets
      self$classifyDroplets() # step 4 - classify droplets as mutant/wildtype/rain
      #self$reclassifyLowMt()  # step 5 - reanalyze low mutant frequency wells
    }
  ),
  
  private = list(
    debug = NULL,
    plateData = NULL,     # the droplets data in each well
    plateMeta = NULL,     # metadata for each well
    datasetName = NULL,   # name of dataset
    status = NULL,        # status of analysis of the plate
    
    wellsSuccess = function() {
      dplyr::filter_(private$plateMeta, quote(success))[['well']]
    },
    
    wellsFailed = function() {
      dplyr::filter_(private$plateMeta, quote(!success))[['well']]
    }
  )
)

#' @export
print.ddpcr_plate <- function(x, ...) {
  cat("Dataset name: ", x$getName(), "\n", sep = "")
  cat("Analysis status: ", x$getStatus(), "\n", sep = "")
  cat("Drops data:\n", sep = "")
  cat(str(x$getData()))
  cat("Plate meta data:\n", sep = "")
  cat(str(x$getMeta()))
}

# dhorizon <- Plate$new("horizon")$analyze()
# dhorizon2 <- Plate$new(p = dhorizon)
# dhorizon3 <- Plate$new()$load("inst/sampledata/dhorizon")
# dthy <- Plate$new()$load("inst/sampledata/thy")
# mini <- loadPlate("inst/sampledata/mini141")