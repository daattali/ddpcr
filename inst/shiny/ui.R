library(shiny)
library(ddpcrS3)

shinyUI(tagList(
  shinyjs::useShinyjs(),
  #   tags$head(includeScript(file.path(staticDir, 'message-handler.js')),
  #             includeScript(file.path(staticDir, 'helper-script.js')),
  #             includeCSS(file.path(staticDir, 'style.css'))
  #   ),
  navbarPage(
    title = tags$b("ddPCR Analysis"),
    windowTitle = "ddPCR Analysis",
    id = "mainNav",
    inverse = TRUE,
  
    source("tab-dataset.R", local = TRUE)$value,
    source("tab-analyze.R", local = TRUE)$value,
    source("tab-plot.R", local = TRUE)$value
  )
))
  
  