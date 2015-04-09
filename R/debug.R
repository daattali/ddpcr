# debugging functions that should not be included when the package is ready
Plate$set("public", "setd", function(d) {
  private$plateData <- d
  invisible(self)
})
Plate$set("public", "setm", function(m) {
  private$plateMeta <- m
  invisible(self)
})
Plate$set("public", "save", function(file) {
  #TODO(daattali) convert data to factors to save space?
  object <- list(
    plateData = self$getData(),
    plateMeta = self$getMeta(),
    datasetName = self$getName(),
    status = self$getStatus(),
    debug = TRUE
  )
  saveRDS(object = object, file = sprintf("%s.rds", file))
  
  invisible(self)
})
Plate$set("public", "load", function(file) {
  # if save convets to factor, then make sure to reuse all the columns/rows here
  object <- readRDS(file = sprintf("%s.rds", file))
  self$setm(object[['plateMeta']])
  self$setd(object[['plateData']])
  private$datasetName <- object[['datasetName']]
  private$status <- object[['status']]
  
  invisible(self)
})

#' @export
loadPlate <- function(file) {
  Plate$new()$load(file)
}