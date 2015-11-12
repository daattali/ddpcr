# ddPCR R package - Dean Attali 2015
# --- Analyze tab UI --- #

tabPanel(
  title = "Analyze",
  id    = "analyzeTab",
  value = "analyzeTab",
  icon  = icon("calculator"),
  
  conditionalPanel(
    condition = "output.datasetChosen", 
    div(id = "analyzeTabContent",
      p("Analyze the droplets to classify each droplet into a group.", br(),
        "This may take several minutes depending on the number of wells."),
      
      withBusyIndicator(
        actionButton(
          "analyzeBtn",
          "Run analysis",
          class = "btn-primary btn-lg"
        )
      ),
      pre(id = "analyzeProgress")
    ),
    
    hidden(
      div(
        id = "analyzeNextMsg",
        class = "next-msg",
        "The data has been analyzed, you can",
        actionLink("toResults", "continue to Results")
      )
    )    
  )
)