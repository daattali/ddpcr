library(shiny)
library(shinyjs)
library(ddpcr)

# allow uploading files up to 100MB
options(shiny.maxRequestSize = 100*1024^2) 

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

  output$datasetDesc <- renderUI({
    if (is.null(dataValues$plate)) {
      div(id = "header-select", "Please select a dataset to begin")
    } else {
      div(
        dataValues$plate %>% name, br(),
        "Type:", dataValues$plate %>% type, br(),
        dataValues$plate %>% wells_used %>% length, " wells; ",
        dataValues$plate %>% plate_data %>% nrow %>% format(big.mark = ","), " droplets"
      )
    }
  })
  
  # When a main tab or secondary tab is switched, clear the error message
  observe({
    input$mainNav
    input$datasetTabs
    
    hide("errorDiv")
  })
  
  # --- Dataset tab --- #
  
  # when data files are being chosen
  observeEvent(input$uploadDataFiles, ignoreNULL = FALSE, {
    toggleState("uploadFilesBtn", !is.null(input$uploadDataFiles))
  })
  
  # when "Upload data" button is clicked
  observeEvent(input$uploadFilesBtn, {
    # User-experience stuff
    disable("uploadFilesBtn")
    show("uploadFilesMsg")
    on.exit({
      enable("uploadFilesBtn")
      hide("uploadFilesMsg")
    })
    hide("errorDiv")    

    tryCatch({
      dataFiles <- input$uploadDataFiles %>% fixUploadedFilesNames
      metaFile <- input$uploadMetaFile %>% fixUploadedFilesNames
      
      # read plate using uploaded files
      dataValues$plate <-
        new_plate(data_files = dataFiles$datapath,
                  meta_file = metaFile$datapath,
                  type = WTNEGBRAF)
    
      output$datasetChosen <- reactive({ TRUE })
      updateTabsetPanel(session, "mainNav", "settingsTab")
    }, error = errorFunc)
  })
  
  # when a file is chosen to load a saved dataset
  observeEvent(input$loadFile, { 
    # User-experience stuff
    show("loadFileMsg")
    on.exit({
      hide("loadFileMsg")
    })
    hide("errorDiv")
    
    tryCatch({
      file <- input$loadFile %>% fixUploadedFilesNames
      dataValues$plate <- load_plate(file$datapath)
      
      output$datasetChosen <- reactive({ TRUE })
      updateTabsetPanel(session, "mainNav", "settingsTab")
    }, error = errorFunc)

  })
  
  # --- General --- #
  
  # whenever the plate gets updated
  observeEvent(dataValues$plate, {
    
    # update the plate summary
    output$analyzePlateData <- renderPrint({
      dataValues$plate
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
    
    # update the plot that shows what wells are available
    p <-
      ddpcr:::plot.ddpcr_plate(
        dataValues$plate, show_drops = FALSE,
        bg_unused = "black", bg_failed = "white", bg_plot = "#f8f8f8",
        text_size_row_col = 20, xlab = NULL, ylab = NULL
      )
    p_width <- 50 * (1 + attr(p, 'ddpcr_cols'))
    p_height <- 50 * (1 + attr(p, 'ddpcr_rows'))
    output$wellsUsedPlot <- renderPlot({p}, width = p_width, height = p_height)
  })  
  
  # --- Settings tab --- #

  # plot that helps select wells is clicked
  observeEvent(input$wellsUsedPlotClick, {
    clickedWell <- sprintf(
      "%s%02d",
      input$wellsUsedPlotClick$panelvar2,
      input$wellsUsedPlotClick$panelvar1 %>% as.integer
    )

    if (!clickedWell %in% (dataValues$plate %>% wells_used)) {
      return(NULL)
    }
    
    oldValue <- input$settingsSubset
    if (grepl(clickedWell, oldValue)) {
      newValue <- gsub(paste0(clickedWell, "(, )?"), "", oldValue)
    } else {
      newValue <- paste0(oldValue, clickedWell, ", ")      
    }
    updateTextInput(session, "settingsSubset", value = newValue)
  })
  
  # update settings button is clicked
  observeEvent(input$updateBasicSettings, {
    # User-experience stuff
    disable("updateBasicSettings")
    show("updateBasicSettingsMsg")
    on.exit({
      enable("updateBasicSettings")
      hide("updateBasicSettingsMsg")
    })
    hide("errorDiv")    
    
    tryCatch({
      if (type(dataValues$plate) != input$settingsPlateType &&
          input$settingsPlateType != "") {
        dataValues$plate <- ddpcr::reset(dataValues$plate, input$settingsPlateType)
      }
      name(dataValues$plate) <- input$settingsName
      x_var(dataValues$plate) <- input$settingsXvar
      y_var(dataValues$plate) <- input$settingsYvar
      if (type(dataValues$plate) == CROSSHAIR_THRESHOLDS) {
        x_threshold(dataValues$plate) <- input$settingsXThreshold
        y_threshold(dataValues$plate) <- input$settingsYThreshold
      }
      
      show("updateBasicSettingsDone")
      hide(id = "updateBasicSettingsDone", anim = TRUE,
           animType = "fade", time = 0.5, delay = 3)
    }, error = errorFunc)
  })
  
  observeEvent(input$updateSubsetSettings, {
    dataValues$plate <- subset(dataValues$plate, input$settingsSubset)
  })   
  
  # When the plate changes, update the advanced settings UI
  observeEvent(dataValues$plate, {
    plate <- dataValues$plate
    output$advancedSettings <- renderUI({
      
      # loop through the parameters and create an input for each one
      lapply(
        plate %>% params %>% names,
        function(major_name) {
          lapply(
            plate %>% params %>% .[[major_name]] %>% names,
            function(minor_name) {
              param_name <- sprintf("%s::%s", major_name, minor_name)
              
              params_ignore <- c(
                "GENERAL::X_VAR", "GENERAL::Y_VAR",
                "CLASSIFY::X_THRESHOLD", "CLASSIFY::Y_THRESHOLD"
              )
              
              if (param_name %in% params_ignore) {
                return(NULL)
              }
              
              param_val <- plate %>% params %>% .[[c(major_name, minor_name)]] 
              param_id <- sprintf("advanced_setting_param_%s__%s", major_name, minor_name)
              
              
              # in order to ensure the correct type for each variable,
              # we need to make sure that boolean/numeric/string parameters
              # are rendered in an appropriate input type. Otherwise if I used
              # textInput for all of them, then all parameters would be
              # converted to string when reading them back.
              if (param_val %>% is.logical) {
                input_type <- checkboxInput
              } else if (param_val %>% is.numeric) {
                input_type <- numericInput
              } else {
                input_type <- textInput
              }
              do.call(input_type, list(param_id, param_name, param_val))
            }
          )
        }
      )
    })
  })
  
  # When the advanced settings update button is clicked,
  # check all advanced settings and save them
  observeEvent(input$updateAdvancedSettings, {
    # User-experience stuff
    disable("updateAdvancedSettings")
    show("updateAdvancedSettingsMsg")
    on.exit({
      enable("updateAdvancedSettings")
      hide("updateAdvancedSettingsMsg")
    })
    hide("errorDiv")   
    
    tryCatch({
      advanced_param_regex <- "^advanced_setting_param_(.*)__(.*)$"
      all_params <- 
        grep(advanced_param_regex, names(input), value = TRUE)
      lapply(all_params, function(x) {
        if (!is.null(input[[x]]) && !is.na(input[[x]])) {
          major_name <- gsub(advanced_param_regex, "\\1", x)
          minor_name <- gsub(advanced_param_regex, "\\2", x)
          params(dataValues$plate, major_name, minor_name) <- input[[x]]
        }
      })

      show("updateAdvancedSettingsDone")
      hide(id = "updateAdvancedSettingsDone", anim = TRUE,
           animType = "fade", time = 0.5, delay = 3)
    }, error = errorFunc)
  })
  
  # --- Analyze tab --- #
  
  # analyze button is clicked
  observeEvent(input$analyzeBtn, {
    dataValues$plate <- dataValues$plate %>% analyze
    #updateTabsetPanel(session, "mainNav", "plotTab")
  })
  
  # save button (download dataset) button is clicked
  output$saveBtn <- downloadHandler(
    filename = function() {
      dataValues$plate %>% name %>% normalize_to_rds
    },
    content = function(file) {
      save_plate(dataValues$plate, file)
    }
  )  
  # --- Plot tab --- #
  
  # plot button is clicked
  observeEvent(input$plotBtn, {
    output$plot <- renderPlot({
      plot_params <- 
        sapply(formals(ddpcr:::plot.ddpcr_plate) %>% names,
               function(x) input[[sprintf("plot_param_%s", x)]] ) %>%
        .[!lapply(., is.null) %>% unlist]
      plot_params[['x']] <- dataValues$plate
      do.call(plot, plot_params)
    }, height = "auto")
  })
  
})

# Error handler that gets used in many tryCatch blocks
errorFunc <- function(err) {
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  text("errorMsg", errMessage)
  show("errorDiv", TRUE, "fade")
}

# When files get uploaded, their new filenames are gibberish.
# This function renames all uploaded files to their original names
fixUploadedFilesNames <- function(x) {
  if (is.null(x)) {
    return()
  }
  
  oldNames = x$datapath
  newNames = file.path(dirname(x$datapath),
                       x$name)
  file.rename(from = oldNames, to = newNames)
  x$datapath <- newNames
  x
}
