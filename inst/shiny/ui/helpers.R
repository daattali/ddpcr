# ddPCR R package - Dean Attali 2015
# This file contains various UI helper functions for the shiny app

# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicator <- function(button) {
  id <- button[['attribs']][['id']]
  tagList(
    button,
    span(
      class = "btn-loading-container",
      `data-for-btn` = id,
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    )
  )
}

# Some info to help with the default values of all the droplet params in plot
plotDropsParams <- list(
  "failed" = list(
    name = "Droplets in<br/>failed wells",
    type = c(KRAS, WTNEGBRAF, "ddpcr_plate"),
    show = TRUE,
    col = "Default",
    alpha = 0.1
  ),
  "outlier" = list(
    name = "Outlier droplets",
    type = c(KRAS, WTNEGBRAF, "ddpcr_plate"),
    show = FALSE,
    col = "orange",
    alpha = 1
  ),
  "empty" = list(
    name = "Empty droplets",
    type = c(KRAS, WTNEGBRAF, CROSSHAIR_THRESHOLDS, "ddpcr_plate"),
    show = FALSE,
    col = "Default",
    alpha = 0.1
  ),
  "rain" = list(
    name = "Rain droplets",
    type = c(KRAS, WTNEGBRAF),
    show = TRUE,
    col = "black",
    alpha = 0.1
  ),
  "positive" = list(
    name = "Wild type droplets",
    type = c(KRAS, WTNEGBRAF),
    show = TRUE,
    col = "green3",
    alpha = 0.1
  ),
  "negative" = list(
    name = "Mutant droplets",
    type = c(KRAS, WTNEGBRAF),
    show = TRUE,
    col = "purple3",
    alpha = 0.1
  ),
  "x_positive" = list(
    name = "X+Y- droplets",
    type = c(CROSSHAIR_THRESHOLDS),
    show = TRUE,
    col = "green3",
    alpha = 0.1
  ),
  "y_positive" = list(
    name = "X-Y+ droplets",
    type = c(CROSSHAIR_THRESHOLDS),
    show = TRUE,
    col = "blue",
    alpha = 0.1
  ),
  "both_positive" = list(
    name = "X+Y+ droplets",
    type = c(CROSSHAIR_THRESHOLDS),
    show = TRUE,
    col = "orange",
    alpha = 0.1
  )
)

# Clours to let user select from in various inputs fields
allCols <- sort(c(
  "black", "blue", "green" = "green3", "orange", "purple" = "purple3",
  "red", "silver", "teal", "yellow", "brown", "gold", "gray" = "gray7", "white"
))
allColsDefault <- c("Default", allCols)