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
  tags$pre(textOutput("anayzePlateData"))
)