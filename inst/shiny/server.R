library(shiny)
library(shinyjs)
library(ddpcr)

# allow uploading files up to 100MB
options(shiny.maxRequestSize = 100*1024^2) 

source(file.path("server", "helpers.R"))

shinyServer(function(input, output, session) {

  # --- General --- #
  
  # reactive values we will use throughout the app
  dataValues <- reactiveValues(
    plate = NULL
  )

  # we need to have a quasi-variable flag to indicate whether or not
  # we have a dataset to work with or if we're waiting for dataset to be chosen
  output$datasetChosen <- reactive({ FALSE })
  outputOptions(output, 'datasetChosen', suspendWhenHidden = FALSE)

  # save button (download dataset) button is clicked
  output$saveBtn <- downloadHandler(
    filename = function() {
      dataValues$plate %>% name %>% normalize_to_rds
    },
    content = function(file) {
      save_plate(dataValues$plate, file)
    }
  ) 
  
  # When a main or secondary tab is switched
  observe({
    input$mainNav
    input$datasetTabs
    input$settingsTabs
    input$resultsTabs
    
    # don't show the dataset description in About tab
    toggle(id = "headerDatasetDesc",
           condition = input$mainNav != "aboutTab")
    
    # clear the error message
    hide("errorDiv")
  })

  # whenever the plate gets updated
  observeEvent(dataValues$plate, {
    # update the plate description
    output$datasetDescName <- renderText({
      dataValues$plate %>% name
    })
    output$datasetDescNumWells <- renderText({
      dataValues$plate %>% wells_used %>% length
    })
    output$datasetDescNumDrops <- renderText({
      dataValues$plate %>% plate_data %>% nrow %>% format(big.mark = ",")
    })  
    
    # update the settings
    updateSelectInput(session, "settingsPlateType", selected = dataValues$plate %>% type)
    updateTextInput(session, "settingsName", value = dataValues$plate %>% name)
    updateTextInput(session, "settingsXvar", value = dataValues$plate %>% x_var)
    updateTextInput(session, "settingsYvar", value = dataValues$plate %>% y_var)
    if (type(dataValues$plate) == CROSSHAIR_THRESHOLDS) {
      updateTextInput(session, "settingsXThreshold", value = dataValues$plate %>% x_threshold)
      updateTextInput(session, "settingsYThreshold", value = dataValues$plate %>% y_threshold)
    }
    updateTextInput(session, "settingsSubset", value = "")
    
    # update plot settings
    hide(selector = "[data-ddpcr-type]")
    show(selector = sprintf("[data-ddpcr-type~=%s]", dataValues$plate %>% type))
    if (type(dataValues$plate) == CROSSHAIR_THRESHOLDS) {
      updateSelectInput(session, "plotParamDropShow-empty", selected = "TRUE")
    }
    updateTextInput(session, "plotParam_xlab", value = dataValues$plate %>% x_var)
    updateTextInput(session, "plotParam_ylab", value = dataValues$plate %>% y_var)
    
  })  
  
  # include logic for each tab
  source(file.path("server", "tab-dataset.R"),  local = TRUE)$value
  source(file.path("server", "tab-settings.R"), local = TRUE)$value
  source(file.path("server", "tab-analyze.R"),  local = TRUE)$value
  source(file.path("server", "tab-results.R"),  local = TRUE)$value
})


