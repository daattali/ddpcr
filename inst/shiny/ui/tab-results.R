allCols <- sort(colours(TRUE))
allColsDefault <- c("Default", allCols)

tabPanel(
  title = "Results",
  id    = "resultsTab",
  value = "resultsTab",
  name  = "resultsTab",
  icon  = icon("bar-chart"),
  
  conditionalPanel(
    condition = "output.datasetChosen",
  
    tabsetPanel(
      id = "resultsTabs", type = "tabs",    
      
      # Plate data tab
      tabPanel(
        title = "Plate summary",
        id = "metaTab",
        br(),
        downloadButton("saveMetaBtn", "Download plate summary"),
        br(), br(),
        DT::dataTableOutput("metaTable")
      ),
      
      # Droplets data tab
      tabPanel(
        title = "Droplets data",
        id = "dropletsTab",
        br(),
        downloadButton("saveDropletsBtn", "Download droplets data"),
        br(), br(),
        fixedRow(
          column(8,
            DT::dataTableOutput("dropletsTable")
          ),
          column(4,
            br(),
            div(id = "clustersMappingOuter",
              span(id = "clustersMappingInner",
                  div(id = "clustersTitle", "Clusters"),
                  uiOutput("clustersMapping")
              )
            )
          )
        )
      ),

      # Plot tab
      tabPanel(
        title = "Plot",
        id = "plotTab",   
        
        br(),
        actionButton(
          "plotBtn",
          "Plot",
          class = "btn-primary"
        ),
        downloadButton("downloadPlot", "Save plot"),        
        div(id = "plotOptionsSection",
            div(id = "plotOptionsTitle", "Plot options"),
            tabsetPanel(
              id = "plotParamsTabs", type = "pills",    
              
              tabPanel(
                title = "General",
                id = "plotGeneralTab",
                br(),
                fixedRow(
                  column(
                    4,
                    selectInput(
                      "plotParamSubsetType", NULL,
                      c("Show all wells" = "all",
                        "Select specific wells" = "wells",
                        "Select specific samples" = "samples"),
                      selected = "all"),
                    conditionalPanel(
                      "input.plotParamSubsetType == 'wells'",
                      uiOutput("plotParamWellsSelect")
                    ),
                    conditionalPanel(
                      "input.plotParamSubsetType == 'samples'",
                      uiOutput("plotParamSamplesSelect")
                    ),
                    checkboxInput("plotParamIncludeFailed", "Include failed wells", TRUE),
                    checkboxInput("plotParamShowDrops", "Show droplets", TRUE)
                  ),
                  column(
                    4,
                    numericInput("plotParamDropsSize", "Droplets size", 2, 0, 50),
                    selectInput("plotParamDropsCol", "Droplets colour",
                                allCols, "black"),
                    sliderInput("plotParamDropsAlpha", "Droplets transparency",
                                0, 1, 0.1, 0.05)
                  ),
                  column(
                    4,
                    checkboxInput("plotParamSuperimpose", "Superimpose all data in one panel", FALSE),
                    checkboxInput("plotParamShowFullPlate", "Show full plate", FALSE),
                    conditionalPanel(
                      sprintf("input.settingsPlateType == '%s'", CROSSHAIR_THRESHOLDS),
                      checkboxInput("plotParamShowThresholds", "Show threshold borders", TRUE),
                      selectInput("plotParamThresholdsCol", "Threshold borders colour",
                                  allCols, "black")
                    ),
                    conditionalPanel(
                      sprintf("input.settingsPlateType == '%s' || input.settingsPlateType == '%s'",
                              KRAS, WTNEGBRAF),
                      checkboxInput("plotParamShowFreq", "Show mutant frequency", TRUE),
                      numericInput("plotParamMutFreqSize", "Mutant frequency text size", 4, 0, 50)
                    )
                  )
                )
              ),
              
              tabPanel(
                title = "Droplets",
                id = "plotDropsTab",
                br(),
                conditionalPanel(
                  "input.plotParamShowDrops",
                  "dsfds"
                ),
                conditionalPanel(
                  "!input.plotParamShowDrops",
                  h4(strong("Turn on \"Show droplets\" in the General options",
                                 "to see more droplet options."))
                )
              ),
              tabPanel(
                title = "Background colours",
                id = "plotTab"
              )
            )
        )
      )
    )
  )
)
