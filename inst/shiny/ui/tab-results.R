allCols <- sort(c(
  "aqua", "black", "blue", "fuchsia", "gray", "green3", "lime", "maroon",
  "navy", "olive", "orange", "purple3", "red", "silver", "teal", "white",
  "yellow", "brown", "gold"
))
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
        id    = "metaTab",
        value = "metaTab",
        name  = "metaTab",
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
                                0, 1, 0.1, 0.05, ticks = FALSE)
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
                conditionalPanel(
                  "!input.plotParamShowDrops",
                  h4(strong("Turn on \"Show droplets\" in the General options",
                            "to see more droplet options."))
                ),
                conditionalPanel(
                  "input.plotParamShowDrops",
                  fixedRow(
                    column(
                      width = 3,
                      offset = 6,
                      h3(strong("Colour"))
                    ),
                    column(
                      3,
                      h3(strong("Transparency"))
                    )
                  ),
                  lapply(names(plotDropsParams), function(x) {
                    div(
                      id = sprintf("plotParamsDropRow-%s", x),
                      class = "plotParamsDropRow",
                      `data-drop-type` = paste(plotDropsParams[[x]]$type, collapse = " "),
                      fixedRow(
                        column(
                          4,
                          strong(plotDropsParams[[x]]$name),
                          class = "plotParamDropName"
                        ),
                        column(
                          2,
                          selectInput(sprintf("plotParamDropShow-%s", x),
                                      NULL,
                                      c("Show" = TRUE, "Don't show" = FALSE),
                                      plotDropsParams[[x]]$show)
                        ),
                        column(
                          3,
                          selectInput(sprintf("plotParamDropCol-%s", x),
                                      NULL, allColsDefault, plotDropsParams[[x]]$col)
                        ),
                        column(
                          3,
                          sliderInput(sprintf("plotParamDropAlpha-%s", x), NULL,
                                      0, 1, plotDropsParams[[x]]$alpha, 0.05,
                                      ticks = FALSE)
                        )
                      )
                    )
                  })
                )
              ),
              tabPanel(
                title = "Background colours",
                id = "plotTab"
              )
            )
        ),
        actionButton(
          "plotBtn",
          "Plot",
          class = "btn-primary btn-lg"
        ),
        hidden(downloadButton("downloadPlot", "Save plot")),
        hidden(
          div(id = "mainPlotContainer",
              img(src = "ajax-loader.gif", id = "plotSpinner"),
              plotOutput("mainPlot")
          )
        )
      )
    )
  )
)