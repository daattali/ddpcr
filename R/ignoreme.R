# this function is never called, but is here just to provide dummy usage of
# the shinyjs and DT packages. These packages are used in the Shiny app which
# is not in the main R directory, and CRAN checks complain that they are in the
# DESCRIPTION Imports field if they are not used in the R directory, so this is
# just to keep CRAN checks happy.
nevercalled <- function(filename) {
  ignored <- shinyjs::useShinyjs()
  ignored <- DT::datatable(data.frame())
}