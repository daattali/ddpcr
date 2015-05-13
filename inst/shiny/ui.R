library(shiny)

source("tab-dataset.R")
source("tab-analyze.R")
source("tab-plot.R")

shinyUI(navbarPage(
  # title = "ddPCR Analysis", # creates ghost tab, bug in shiny
  windowTitle = "ddPCR Analysis",
  id = "mainNav",
  inverse = TRUE,
  shinyjs::useShinyjs(),
#   tags$head(includeScript(file.path(staticDir, 'message-handler.js')),
#             includeScript(file.path(staticDir, 'helper-script.js')),
#             includeCSS(file.path(staticDir, 'style.css'))
#   ),
  
#
  getTabDataset(),
  getTabAnalyze(),
  getTabPlot()
))
  
  