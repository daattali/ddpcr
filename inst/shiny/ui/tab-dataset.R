# This file contains the UI for the dataset selection tab

tabPanel(
  title = "Dataset",
  id    = "datasetTab",
  value = "datasetTab",
  name  = "datasetTab",
  icon  = icon("table"),
  
  tabsetPanel(
    id = "datasetTabs", type = "tabs",    
    
    # tab for uploading a new dataset 
    tabPanel(
      title = "Upload new dataset",
      id = "newDatasetTab",
      h3("Upload data from QuantaSoft ",
         helpPopup("You must first export the data from QuantaSoft to <i>.csv</i> (Excel) files")
      ),
      fileInput(
        "uploadDataFiles",
        div("Data files (one file per well)",
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
        div("Metadata file (optional)",
            helpPopup("This is the Excel file exported by QuantaSoft that contains two lines with many variables for every well")),
        multiple = FALSE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      
      actionButton(
        "uploadFilesBtn",
        "Upload data",
        class = "btn-primary"
      ),
      hidden(
        span(id = "uploadFilesMsg",
             "Uploading...",
             class = "btn-msg"
        )
      )
    ),
    
    # tab for loading existing dataset
    tabPanel(
      title = "Load saved dataset",
      id = "loadDatasetTab",
      h3("Upload previously saved data",
         helpPopup("If you've previously used this program to save data, you can load it here")),
      
      fileInput(
        "loadFile",
        "Saved ddPCR file",
        multiple = FALSE,
        accept = c(
          '.rds'
        )
      ),
      hidden(
        span(id = "loadFileMsg",
             "Loading...",
             class = "btn-msg"
        )
      )
    )
  )
)