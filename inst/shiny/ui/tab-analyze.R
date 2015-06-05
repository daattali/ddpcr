# This file contains the UI for the data analysis tab

tabPanel(
  title = "Analyze",
  id    = "analyzeTab",
  value = "analyzeTab",
  icon  = icon("calculator"),
  
  div(id = "analyzeTabContent",
    conditionalPanel(
      condition = "output.datasetChosen", 
      
      p("Analyze the droplets to classify each droplet into a group.", br(),
        "This may take several minutes depending on the number of wells."),
      actionButton(
        "analyzeBtn",
        "Run analysis",
        class = "btn-primary btn-lg"
      ),
      pre(id = "analyzeProgress")
    )
  )
)