#' @export
shine <- function() {
  shiny::runApp(system.file("shiny", package = "ddpcr"), display.mode = "normal")
}