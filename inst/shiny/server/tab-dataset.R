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