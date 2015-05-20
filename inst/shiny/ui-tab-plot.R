# This file contains the UI for the tab that plots the dataset

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