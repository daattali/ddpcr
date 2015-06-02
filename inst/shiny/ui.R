# This is the main UI file that initializes the UI and aggregates all the tabs

library(shiny)
library(shinyjs)
library(ddpcr)

source("ui-helper-helpPopup.R")

tagList(
  useShinyjs(),
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
    header = column(12,
                    div(id = "headerDatasetDesc",
                      conditionalPanel(
                        condition = "!output.datasetChosen",
                        div(id = "datasetDescSelect",
                            "Please select a dataset to begin")
                      ),
                      conditionalPanel(
                        condition = "output.datasetChosen",
                        textOutput("datasetDescName"),
                        div(id = "datasetDescSummary",
                            "Plate with",
                            textOutput("datasetDescNumWells", inline = TRUE),
                            "wells and",
                            textOutput("datasetDescNumDrops", inline = TRUE),
                            "droplets"
                        ),
                        downloadButton('saveBtn', 'Save data')
                      ),
                      hr(class = "small")
                    )
             ),
    source("ui-tab-dataset.R",  local = TRUE)$value,
    source("ui-tab-settings.R", local = TRUE)$value,
    source("ui-tab-analyze.R",  local = TRUE)$value,
    source("ui-tab-plot.R",     local = TRUE)$value,
    
    footer = 
      column(12,
          hidden(
          div(id = "errorDiv",
              div(icon("exclamation-circle"), tags$b("Error: "), span(id = "errorMsg"))
          )
        )
      )
  )
)
