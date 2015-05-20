tabPanel(
  title = "Plot",
  id = "plotTab",
  value = "plotTab",
  
  actionButton(
    "plotBtn",
    "Plot",
    class = "btn-primary"
  ),
  plotOutput("plot")
)