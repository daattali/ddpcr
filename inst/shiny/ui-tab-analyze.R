# This file contains the UI for the data analysis tab

tabPanel(
  title = "Analyze",
  id = "analyzeTab",
  value = "analyzeTab",
  
  actionButton(
    "analyzeBtn",
    "Run analysis",
    class = "btn-primary"
  ),
  br(), br(),
  downloadButton('saveBtn', 'Save data'),
  tags$pre(textOutput("analyzePlateData"))
)