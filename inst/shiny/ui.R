# ddPCR R package - Dean Attali 2015
# --- Main UI file for shiny app --- #

library(shiny)
library(shinyjs)
library(ddpcr)

source(file.path("ui", "helpers.R"))

tagList(
  shinydisconnect::disconnectMessage2(),
  useShinyjs(),
  tags$head(
    tags$script(src = "ddpcr.js"),
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  div(id = "loading-content", "Loading...",
      img(src = "ajax-loader-bar.gif")),
  
  navbarPage(
    title = tags$b("ddPCR Analysis"),
    windowTitle = "ddPCR Analysis",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    collapsible = TRUE,
    header = source(file.path("ui", "header.R"),  local = TRUE)$value,
    
    # include the UI for each tab
    source(file.path("ui", "tab-dataset.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-settings.R"), local = TRUE)$value,
    source(file.path("ui", "tab-analyze.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-results.R"),  local = TRUE)$value,
    source(file.path("ui", "tab-about.R"),    local = TRUE)$value,
    
    footer = 
      column(12,
        hidden(
          div(id = "errorDiv",
            div(icon("exclamation-circle"),
                tags$b("Error: "),
                span(id = "errorMsg")
            )
          )
        )
      )
  )
)
