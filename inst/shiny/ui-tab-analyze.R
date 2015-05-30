# This file contains the UI for the data analysis tab

tabPanel(
  title = "Analyze",
  id = "analyzeTab",
  value = "analyzeTab",
  
  conditionalPanel(
    condition = "output.datasetChosen",  
  
    actionButton(
      "analyzeBtn",
      "Run analysis",
      class = "btn-primary"
    ),
    br(), br(),
    tags$pre(textOutput("analyzePlateData"))
  )
)