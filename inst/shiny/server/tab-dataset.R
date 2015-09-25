# ddPCR R package - Dean Attali 2015
# --- Dataset tab server --- #

# only enable the upload buttons when their corresponding input has a file selected
observeEvent(input$uploadDataFiles, ignoreNULL = FALSE, {
  toggleState("uploadFilesBtn", !is.null(input$uploadDataFiles))
})
observeEvent(input$loadFile, ignoreNULL = FALSE, {
  toggleState("loadFileBtn", !is.null(input$loadFile))
})

# when "Upload data" button is clicked
observeEvent(input$uploadFilesBtn, {
  withBusyIndicator("uploadFilesBtn", {
    dataFiles <- input$uploadDataFiles %>% fixUploadedFilesNames
    metaFile <- input$uploadMetaFile %>% fixUploadedFilesNames
    
    # read plate using uploaded files
    dataValues$plate <-
      new_plate(data_files = dataFiles$datapath,
                meta_file = metaFile$datapath,
                type = plate_types$fam_positive_pnpp)
    
    output$datasetChosen <- reactive({ TRUE })
    show("datasetNextMsg")
  })
})

# when a file is chosen to load a saved dataset
observeEvent(input$loadFileBtn, {
  withBusyIndicator("loadFileBtn", {
    file <- input$loadFile %>% fixUploadedFilesNames
    dataValues$plate <- load_plate(file$datapath)
    
    output$datasetChosen <- reactive({ TRUE })
    show("datasetNextMsg")
  })
})

# load the sample dataset
observeEvent(input$loadSampleBtn, {
  withBusyIndicator("loadSampleBtn", {
    dataValues$plate <- new_plate(dir = sample_data_dir(),
                                  type = plate_types$fam_positive_pnpp)
    
    output$datasetChosen <- reactive({ TRUE })
    show("datasetNextMsg")
  })
})

# change to settings tab when clicking on link
observeEvent(input$toSettings,
  updateTabsetPanel(session, "mainNav", "settingsTab")
)