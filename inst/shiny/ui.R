# This is the main UI file that initializes the UI and aggregates all the tabs

library(shiny)
library(ddpcrS3)
library(shinyjs)

shinyUI(tagList(
  shinyjs::useShinyjs(),
  tags$head(
    includeScript(file.path("www", 'ddpcr.js')),
    #             includeScript(file.path(staticDir, 'helper-script.js')),
    includeCSS(file.path("www", 'style.css'))
  ),
  
  navbarPage(
    title = tags$b("ddPCR Analysis"),
    windowTitle = "ddPCR Analysis",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    header = column(12, uiOutput("datasetDesc")),
    
    source("ui-tab-dataset.R",  local = TRUE)$value,
    source("ui-tab-settings.R", local = TRUE)$value,
    source("ui-tab-analyze.R",  local = TRUE)$value,
    source("ui-tab-plot.R",     local = TRUE)$value,
    
    footer = 
      column(12,
        hidden(
          div(id = "errorDiv",
              div(tags$b("Error: "), span(id = "errorMsg"))
          )
        )
      )
  )
))

