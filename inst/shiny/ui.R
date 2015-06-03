# This is the main UI file that initializes the UI and aggregates all the tabs

library(shiny)
library(shinyjs)
library(ddpcr)

source(file.path("ui", "helpers.R"))

tagList(
  useShinyjs(),
  tags$head(
    includeScript(file.path("www", 'ddpcr.js')),
    includeCSS(file.path("www", 'style.css'))
  ),
  
  navbarPage(
    title = tags$b("ddPCR Analysis"),
    windowTitle = "ddPCR Analysis",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    header = source(file.path("ui", "header.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-dataset.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-settings.R"), local = TRUE)$value,
    source(file.path("ui", "tab-analyze.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-results.R"),  local = TRUE)$value,
    
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
