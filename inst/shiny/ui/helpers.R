helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle"))
}

plotDropsParams <- list(
  "failed" = list(
    name = "Droplets in failed wells",
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