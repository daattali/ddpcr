# ddPCR R package - Dean Attali 2015
# --- Dataset tab UI --- #

tabPanel(
  title = "Dataset",
  id    = "datasetTab",
  value = "datasetTab",
  name  = "datasetTab",
  icon  = icon("table"),
  
  tabsetPanel(
    id = "datasetTabs", type = "tabs",    
    
    # tab for uploading a new dataset ----
    tabPanel(
      title = "Upload new dataset",
      id = "newDatasetTab",
      h3(strong("Upload data from QuantaSoft "),
         helpPopup("You must first export the data from QuantaSoft to <i>.csv</i> (Excel) files")
      ),
      br(),
      fileInput(
        "uploadDataFiles",
        div("Data files",
            helpPopup("These are all the <i>_Amplitude</i> files exported by QuantaSoft")),
        multiple = TRUE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      fileInput(
        "uploadMetaFile",
        div("Main results file (optional)",
            helpPopup("This is the Excel file exported by QuantaSoft that contains the main results for every well.")),
        multiple = FALSE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      
      withBusyIndicator(
        actionButton(
          "uploadFilesBtn",
          "Upload data",
          class = "btn-primary"
        )
      )
    ),
    
    # tab for loading existing dataset ----
    tabPanel(
      title = "Load saved dataset",
      id = "loadDatasetTab",
      h3(strong("Upload previously saved data"),
         helpPopup("If you've previously used this program to save data, you can load it here")),
      br(),
      fileInput(
        "loadFile",
        "Saved ddPCR file",
        multiple = FALSE,
        accept = c(
          '.rds'
        )
      ),
      withBusyIndicator(
        actionButton(
          "loadFileBtn",
          "Load data",
          class = "btn-primary"
        )
      )
    ),
    
    # tab for loading sample dataset ----
    tabPanel(
      title = "Use sample dataset",
      id = "sampleDatasetTab",
      h3(strong("Use sample dataset")),
      br(),
      withBusyIndicator(
        actionButton(
          "loadSampleBtn",
          "Load data",
          class = "btn-primary"
        )
      )
    )
  ),
  
  hidden(
    div(
      id = "datasetNextMsg",
      class = "next-msg",
      "The data has been loaded, you can continue to",
      actionLink("toSettings", "Settings")
    )
  )
)