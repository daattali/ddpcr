# ddPCR R package - Dean Attali 2015
# This file contains various server helper functions for the shiny app

# For better user experience, when a button is pressed this will disable
# the button while the action is being taken, show a loading indicator, and
# show a checkmark when it's done. If an error occurs, show the error message. 
# This works best if the given button was set up with `withBusyIndicator` in
# the UI (otherwise it will only disable the button and take care of errors,
# but won't show the loading/done indicators)
withBusyIndicator <- function(buttonId, expr) {
  
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  disable(buttonId)
  show(selector = loadingEl)
  hide(selector = doneEl)
  hide("errorDiv")
  on.exit({
    enable(buttonId)
    hide(selector = loadingEl)
  })

  tryCatch({
    value <- expr
    show(selector = doneEl)
    delay(2000, hide(selector = doneEl, anim = TRUE, animType = "fade",
         time = 0.5))
    value
  }, error = errorFunc)
}

# Error handler that gets used in many tryCatch blocks
errorFunc <- function(err) {
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  html("errorMsg", errMessage)
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

# convet a column name to a nicer human readable version by using
# spaces instead of underscores and capitalizing first letter
humanFriendlyNames <- function(colnames) {
  paste0(toupper(substring(colnames, 1, 1)),
         substring(gsub("_", " ", colnames), 2))
}