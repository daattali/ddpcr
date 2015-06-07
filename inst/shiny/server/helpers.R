# ddPCR R package - Dean Attali 2015
# This file contains various server helper functions for the shiny app

# For better user experience, when a button is pressed this will disable
# the button while the action is being taken, show a loading indicator, and
# show a checkmark when it's done. If an error occurs, show the error message. 
# This only works if the given button was set up with `withBusyIndicator` in
# the UI
withBusyIndicator <- function(buttonId, expr) {
  
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  disable(buttonId)
  show(selector = loadingEl)
  hide(selector = doneEl)
  hide("errorDiv")  
  
  tryCatch({
    expr
    show(selector = doneEl)
    hide(selector = doneEl, anim = TRUE, animType = "fade",
         time = 0.5, delay = 3)
  },
  error = errorFunc,
  finally = {
    enable(buttonId)
    hide(selector = loadingEl)
  })
}

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