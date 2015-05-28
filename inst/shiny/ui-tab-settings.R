# This file contains the UI for the settings tab

tabPanel(
  title = "Settings",
  id = "settingsTab",
  value = "settingsTab",
  name = "settingsTab",

  conditionalPanel(
    condition = "output.datasetChosen",
    tabsetPanel(
      id = "datasetTabs", type = "tabs",    
      
      # Basic settings tab
      tabPanel(
        title = "Basic",
        id = "basicSettingsTab",
        br(),
        textInput(
          "settingsSubset",
          span(
            "Keep only certain wells ",
            helpPopup(
              content = paste("You can select multiple wells using aspecial ",
              "\"range notation\". Non-adjacent wells can be specified by separating",
              "them with commas. Adjacent wells can be selected with the colon",
              "operator, which will select all wells in the rectangle defined by",
              "the two end-points. For example, \"B01, C04:D06, F10\" will select",
              "wells B01, C04, C05, C06, D04, D05, D06, F10."),
              title = "Range notation")),
          value = ""
        ),
        actionLink("settingsShowAllWells", "Show all available wells"), br(),
        hidden(
          div(id = "settingsAllWells", 
              span("White wells are available in this dataset. Click any well to add it to the list above."),
              plotOutput("wellsUsedPlot", click = "wellsUsedPlotClick",
                         height="auto",width="auto")
          )),
        
        br(), br(),
        textInput("settingsXvar", "Dye along X axis", ""),
        textInput("settingsYvar", "Dye along Y axis", ""),
        
        br(),
        actionButton(
          "updateSettings",
          "Update",
          class = "btn-primary"
        )
      ),
      
      # Advanced settings tab
      tabPanel(
        title = "Advanced",
        id = "advancedSettingsTab",
        br(),
        h4("These are advanced options, only use them if you know what you're doing."),
        uiOutput("advancedSettings"),  
        actionButton(
          "updateAdvancedSettings",
          "Update Advanced Settings",
          class = "btn-primary"
        )      
      )
    )
  )
)