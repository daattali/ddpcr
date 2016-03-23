column(
  width = 12,
  div(id = "headerDatasetDesc",
      conditionalPanel(
        condition = "!output.datasetChosen",
        div(id = "datasetDescSelect",
            "Please select a dataset to begin")
      ),
      conditionalPanel(
        condition = "output.datasetChosen",
        textOutput("datasetDescName"),
        span(id = "datasetDescSummary",
            "Plate with",
            textOutput("datasetDescNumWells", inline = TRUE),
            "wells and",
            textOutput("datasetDescNumDrops", inline = TRUE),
            "droplets"
            
        ),
        downloadButton('saveBtn', 'Save data', class = "btn-sm"),
        div(
          id = "plateDirty",
          class = "alert alert-danger",
          strong("Note:"),
          "Some settings have changed, please re-run the analysis for changes to take effect"
        )
      )
  )
)