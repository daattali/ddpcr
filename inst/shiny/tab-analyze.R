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
  tags$pre(textOutput("anayzePlateData"))
  
)