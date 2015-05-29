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
        title = "Basic Settings",
        id = "basicSettingsTab",
        br(),
        
        fixedRow(
          column(6,
            selectInput(
              "settingsPlateType",
              "Plate type",
              c("Wild type negative BRAF" = WTNEGBRAF,
                "KRAS" = KRAS,
                "General (manually set crosshair thresholds)" = CROSSHAIR_THRESHOLDS)
            )
          ),
          column(6,
            textInput("settingsName", "Dataset name", "")
          )
        ),
        fixedRow(
          column(6,
            textInput("settingsXvar", "Dye along X axis", "")
          ),
          column(6,
            textInput("settingsYvar", "Dye along Y axis", "")
          )
        ),
        conditionalPanel(
          sprintf("input.settingsPlateType == '%s'", CROSSHAIR_THRESHOLDS),
          fixedRow(
            column(6,
              numericInput("settingsXThreshold", "X threshold", 5000, min = 0, step = 100)
            ),
            column(6,
              numericInput("settingsYThreshold", "Y threshold", 5000, min = 0, step = 100)
            )
          )
        ),
        br(),
        actionButton(
          "updateBasicSettings",
          "Apply",
          class = "btn-primary"
        ),
        hidden(
          span(id = "updateBasicSettingsMsg",
               "Applying settings...",
               class = "btn-msg"
          )
        ),
        hidden(
          span(id = "updateBasicSettingsDone",
               "Done",
               class = "btn-msg"
          )
        )
      ),

      # Advanced settings tab
      tabPanel(
        title = "Subset Plate",
        id = "subsetSettingsTab",
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
        div(id = "settingsAllWells", 
            span("White wells are available in this dataset. Click any well to add it to the list above."),
            plotOutput("wellsUsedPlot",
                       click = "wellsUsedPlotClick",
                       height = "auto", width = "auto")
        ),
        actionButton(
          "updateSubsetSettings",
          "Apply",
          class = "btn-primary"
        )      
      ),
            
      # Advanced settings tab
      tabPanel(
        title = "Advanced Settings",
        id = "advancedSettingsTab",
        br(),
        h4("These are advanced options, only use them if you know what you're doing."),
        uiOutput("advancedSettings"),  
        actionButton(
          "updateAdvancedSettings",
          "Apply",
          class = "btn-primary"
        ),
        hidden(
          span(id = "updateAdvancedSettingsMsg",
               "Applying settings...",
               class = "btn-msg"
          )
        ),
        hidden(
          span(id = "updateAdvancedSettingsDone",
               "Done",
               class = "btn-msg"
          )
        )        
      )
    )
  )
)