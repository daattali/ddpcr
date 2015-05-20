tabPanel(
  title = "Dataset",
  id = "datasetTab",
  value = "datasetTab",
  name = "datasetTab",
  
  h2("Select a dataset using one of the following options"),

  tabsetPanel(
    id = "datasetTabs", type = "tabs",    
    
    # tab for uploading a new dataset 
    tabPanel(
      title = "Upload new dataset",
      id = "newDatasetTab",
      br(),
      h3("If you have data from QuantaSoft that you want to analyze, upload it here.",
         "(You must first export the data from QuantaSoft to .csv files.)"),
      fileInput(
        "uploadDataFiles",
        "Data files (one file per well)",
        multiple = TRUE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      fileInput(
        "uploadMetaFile",
        "Metadata file (optional; only used to get sample names)",
        multiple = FALSE,
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv'
        )
      ),
      
      selectInput(
        "uploadPlateType",
        "Plate type",
        c(KRAS, WTNEGBRAF, CROSSHAIR_THRESHOLDS) %>% sort,
        WTNEGBRAF
      ),
      
      actionButton(
        "uploadFilesBtn",
        "Upload data",
        class = "btn-primary"
      )
    ),
    
    tabPanel(
      title = "Load saved data",
      id = "loadDatasetTab",
      br(),
      h3("If you've previously used this tool to save data, you can restore it."),
      fileInput(
        "loadFile",
        "Saved ddPCR file",
        multiple = FALSE,
        accept = c(
          '.rds'
        )
      )
    )
  )
)