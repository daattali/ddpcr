# ddPCR R package - Dean Attali 2015
# --- Settings tab UI --- #

tabPanel(
  title = "Settings",
  id = "settingsTab",
  value = "settingsTab",
  name = "settingsTab",
  icon  = icon("cog"),

  conditionalPanel(
    condition = "output.datasetChosen",
    tabsetPanel(
      id = "settingsTabs", type = "tabs",    
      
      # Basic settings tab
      tabPanel(
        title = "Basic Settings",
        id    = "basicSettingsTab",
        value = "basicSettingsTab",
        name  = "basicSettingsTab",
        br(),
        
        div(id = "basicSettingsTabContent",
        fixedRow(
          column(6,
            selectInput(
              "settingsPlateType",
              div("Droplet clusters",
                  helpPopup("Select <strong>(FAM+) / (FAM+HEX+)</strong> or <strong>(HEX+) / (FAM+HEX+)</strong> if your data has a main cluster of double-positive droplets (considered wild type) and a secondary cluster of FAM+ or HEX+ droplets (considered mutant).<br/><br/>Select <strong>Custom gating thresholds</strong> if your data does not fit these models and you want to simply cluster the droplets into 4 quadrants.<br/><br/>Select <strong>Other</strong> if you want to only explore and run pre-processing on your data, but not droplet gating.")
              ),
              c("(FAM+) / (FAM+HEX+)" = plate_types$fam_positive_pnpp,
                "(HEX+) / (FAM+HEX+)" = plate_types$hex_positive_pnpp,
                "Custom gating thresholds" = plate_types$custom_thresholds,
                "Other (no droplet gating)" = plate_types$ddpcr_plate)
            ),
            conditionalPanel(
              sprintf("input.settingsPlateType == '%s' || input.settingsPlateType == '%s'", plate_types$fam_positive_pnpp, plate_types$hex_positive_pnpp),
              actionLink("showPlateTypeExample", "Show example typical well") 
            ),
            conditionalPanel(
              "input.showPlateTypeExample % 2 == 1",
              conditionalPanel(
                sprintf("input.settingsPlateType == '%s'", plate_types$fam_positive_pnpp),
                img(src = "fampositive.png")
              ),
              conditionalPanel(
                sprintf("input.settingsPlateType == '%s'", plate_types$hex_positive_pnpp),
                img(src = "hexpositive.png")
              )              
            )            
          )
        ),
        fixedRow(
          column(6,
                 textInput("settingsName", "Dataset name", "")
          )
        ),
        fixedRow(
          column(6,
            textInput("settingsXvar", "Dye along X axis (Channel 2)", "")
          ),
          column(6,
            textInput("settingsYvar", "Dye along Y axis (Channel 1)", "")
          )
        ),
        conditionalPanel(
          sprintf("input.settingsPlateType == '%s'", plate_types$custom_thresholds),
          fixedRow(
            column(6,
              numericInput("settingsXThreshold", "X threshold", 5000, min = 0, step = 100)
            ),
            column(6,
              numericInput("settingsYThreshold", "Y threshold", 5000, min = 0, step = 100)
            )
          )
        ),
        conditionalPanel(
          sprintf("input.settingsPlateType == '%s' || input.settingsPlateType == '%s'", plate_types$fam_positive_pnpp, plate_types$hex_positive_pnpp),
          fixedRow(
            column(6,
              textInput("settingsPosName", "Identifier for double-positive droplets")
            ),
            column(6,
              textInput("settingsNegName", "Identifier for singly-positive droplets")
            )
          )
        )        
        ),
        br(),
        withBusyIndicator(
          actionButton(
            "updateBasicSettings",
            "Apply",
            class = "btn-primary"
          )
        )
      ),

      # Subset plate tab
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
            p(id = "subsetPlotDesc",
              "Below is a diagram of the plate, with", strong("white wells"),
              "denoting wells that", strong("are available"), "in the current dataset.", br(),
              "Double click any well to add it to the list above, or select multiple",
              "wells by clicking the mouse and dragging it across the plate."),
            plotOutput("wellsUsedPlot",
                       dblclick = "wellsUsedPlotClick",
                       brush = brushOpts("wellsUsedPlotBrush", delay = 5000),
                       height = 450, width = 650)
        ),
        br(),
        div(id = "subsetNotReversibleNote",
          "Note: this action is not reversible",
          helpPopup(paste(
            "Once you subset the dataset to only keep certain wells,",
            "the rest of the data is removed. To retrieve the original data",
            "later, you will need to upload the dataset again."
          ))
        ),
        withBusyIndicator(
          actionButton(
            "updateSubsetSettings",
            "Apply",
            class = "btn-primary"
          )
        )
      ),
            
      # Advanced settings tab
      tabPanel(
        title = "Advanced Settings",
        id = "advancedSettingsTab",
        h3(strong("These are advanced options. Only use them if you know what you're doing.")),
        h4(strong("An explanation of all the settings is available in the"),
           a("algorithm vignette", target = "_blank", href = "https://github.com/daattali/ddpcr/blob/master/vignettes/algorithm.Rmd")),
        br(),
        uiOutput("advancedSettings"),
        actionButton(
          "updateAdvancedSettings",
          "Apply",
          class = "btn-primary"
        ),
        withBusyIndicator(
          actionButton(
            "resetParamsBtn",
            "Reset to default"
          )
        )
      )
    ),
    div(
      id = "settingsNextMsg",
      class = "next-msg",
      "When you are finished with the settings,",
      actionLink("toAnalyze", "continue to Analyze")
    )
  )
)