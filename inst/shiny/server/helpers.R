# Error handler that gets used in many tryCatch blocks
errorFunc <- function(err) {
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  text("errorMsg", errMessage)
  show("errorDiv", TRUE, "fade")
}

# When files get uploaded, their new filenames are gibberish.
# This function renames all uploaded files to their original names
fixUploadedFilesNames <- function(x) {
  if (is.null(x)) {
    return()
  }
  
  oldNames = x$datapath
  newNames = file.path(dirname(x$datapath),
                       x$name)
  file.rename(from = oldNames, to = newNames)
  x$datapath <- newNames
  x
}