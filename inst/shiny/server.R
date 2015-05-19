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
  
  # whenever the plate gets updated
  observeEvent(dataValues$plate, {
    output$anayzePlateData <- renderPrint({
      dataValues$plate
    })
  })  
  
  # when the main navigation bar changes focus to a new tab
  observeEvent(input$mainNav, {
    if (input$mainNav == "analyzeTab") {

    }
  })
  
  # analyze button is clicked
  observeEvent(input$analyzeBtn, {
    dataValues$plate <- dataValues$plate %>% analyze
    updateTabsetPanel(session, "mainNav", "plotTab")
  })
  
  observeEvent(input$plotBtn, {
    output$plot <- renderPlot({
      dataValues$plate %>% plot
    }, height = "auto")
  })
  
  output$saveBtn <- downloadHandler(
    filename = function() {
      dataValues$plate %>% name %>% normalize_to_rds
    },
    content = function(file) {
      save_plate(dataValues$plate, file)
    }
  )  
})

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