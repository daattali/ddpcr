library(shiny)
library(shinyjs)
library(ddpcrS3)

# allow uploading files up to 100MB
options(shiny.maxRequestSize = 100*1024^2) 

shinyServer(function(input, output, session) {
  # reactive values we will use throughout the app
  dataValues <- reactiveValues(
    plate = NULL
  )
  
  # --- Dataset tab --- #
  
  # when data files are being chosen
  observeEvent(input$uploadDataFiles, ignoreNULL = FALSE, {
    shinyjs::toggleState("uploadFilesBtn", !is.null(input$uploadDataFiles))
  })
  
  # when "Upload data" button is clicked
  observeEvent(input$uploadFilesBtn, {
    dataFiles <- input$uploadDataFiles %>% fixUploadedFilesNames
    metaFile <- input$uploadMetaFile %>% fixUploadedFilesNames
    
    # rename the uploaded files to their original names
    # so that we can infer the dataset name from file names
    newNames <- file.path(dirname(dataFiles$datapath),
                          dataFiles$name)
    file.rename(from = dataFiles$datapath,
                to = newNames)
    
    # read plate using uploaded files
    dataValues$plate <-
      new_plate(data_files = newNames,
                meta_file = metaFile$datapath,
                type = input$uploadPlateType)
    
    updateTabsetPanel(session, "mainNav", "analyzeTab")
  })
  
  # when a file is chosen to load a saved dataset
  observeEvent(input$loadFile, {
    file <- input$loadFile %>% fixUploadedFilesNames
    dataValues$plate <- load_plate(file$datapath)
    updateTabsetPanel(session, "mainNav", "analyzeTab")
  })
  
  # --- General --- #
  
  # whenever the plate gets updated
  observeEvent(dataValues$plate, {
    
    # update the plate summary
    output$analyzePlateData <- renderPrint({
      dataValues$plate
    })
    
    # update the settings
    updateTextInput(session, "settingsXvar", value = dataValues$plate %>% x_var)
    updateTextInput(session, "settingsYvar", value = dataValues$plate %>% y_var)
    
    # update the plot that shows what wells are available
    p <-
      ddpcrS3:::plot.ddpcr_plate(
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
    updateTextInput(session, "settingsSubset",
                    value = paste0(input$settingsSubset,
                                   input$wellsUsedPlotClick$panelvar2,
                                   input$wellsUsedPlotClick$panelvar1,
                                   ", "))
  })
  
  # update settings button is clicked
  observeEvent(input$updateSettings, {
    x_var(dataValues$plate) <- input$settingsXvar
    y_var(dataValues$plate) <- input$settingsYvar
    dataValues$plate <- subset(dataValues$plate, input$settingsSubset)
  })
  
  # toggle showing/hiding the well selection plot
  observeEvent(input$settingsShowAllWells, {
    toggle("settingsAllWells")
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
              param_val <- plate %>% params %>% .[[c(major_name, minor_name)]] 
              param_id <- sprintf("advanced_setting_param_%s__%s", major_name, minor_name)
              param_name <- sprintf("%s::%s", major_name, minor_name)
              
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
        sapply(formals(ddpcrS3:::plot.ddpcr_plate) %>% names,
               function(x) input[[sprintf("plot_param_%s", x)]] ) %>%
        .[!lapply(., is.null) %>% unlist]
      plot_params[['x']] <- dataValues$plate
      do.call(plot, plot_params)
    }, height = "auto")
  })
  
})

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