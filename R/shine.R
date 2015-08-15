#' Run the interactive analysis tool (Shiny app) in a web browser
#' 
#' In addition to the method provided in this package, the \code{ddpcr} package
#' also provides an interactive tool that can be used to analyze ddPCR data
#' more easily.
#' @export
shine <- function() {
  shiny::runApp(system.file("shiny", package = "ddpcr"),
                display.mode = "normal",
                launch.browser = TRUE)
}