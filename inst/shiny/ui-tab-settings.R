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
               class = "btn-msg btn-msg-done"
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
              content = paste("You can select multiple wells using a special",
                              "<i>range notation</i>.<br/>Non-adjacent wells can be specified by separating",
                              "them with <strong>commas</strong>. Adjacent wells can be selected with a <strong>colon</strong>,",
                              "which will select all wells in the rectangle defined by",
                              "the two end-points.<br/><br/>Example: \"B01, C04:D06, F10\" will select",
                              "wells <i>B01, C04, C05, C06, D04, D05, D06, F10</i>."),
              title = "Range notation")),
          value = ""
        ),
        div(id = "settingsAllWells", 
            p("Below is a diagram of the plate, with white",
              "wells denoting wells that are available in the current dataset.", br(),
              "Double click any well to add it to the list above, or select multiple",
              "wells by clicking the mouse and dragging it across the plate."),
            plotOutput("wellsUsedPlot",
                       dblclick = "wellsUsedPlotClick",
                       brush = brushOpts("wellsUsedPlotBrush", delay = 5000),
                       height = 450, width = 650)
        ),
        br(),
        p("Note: this action is not reversible. Once you subset the dataset to only keep certain wells,",
          "the rest of the data is removed. To retrieve the original data later, you will need to upload the dataset again.",
          "the dataset again."),
        actionButton(
          "updateSubsetSettings",
          "Apply",
          class = "btn-primary"
        ),
        hidden(
          span(id = "updateSubsetSettingsDone",
               icon("check"), "Done",
               class = "btn-msg btn-msg-done"
          )
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
               class = "btn-msg btn-msg-done"
          )
        )        
      )
    )
  )
)