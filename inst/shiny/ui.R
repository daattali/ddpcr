library(shiny)
library(ddpcrS3)

source("tab-dataset.R")
source("tab-analyze.R")
source("tab-plot.R")

shinyUI(navbarPage(
  title = tags$b("ddPCR Analysis"),
  windowTitle = "ddPCR Analysis",
  id = "mainNav",
  inverse = TRUE,
  shinyjs::useShinyjs(),
#   tags$head(includeScript(file.path(staticDir, 'message-handler.js')),
#             includeScript(file.path(staticDir, 'helper-script.js')),
#             includeCSS(file.path(staticDir, 'style.css'))
#   ),
  
  getTabDataset(),
  getTabAnalyze(),
  getTabPlot()
))
  
  