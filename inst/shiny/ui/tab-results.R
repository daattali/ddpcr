# ddPCR R package - Dean Attali 2015
# --- Results tab UI --- #

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
      
      # Plate data tab ----
      tabPanel(
        title = "Plate summary",
        id    = "metaTab",
        value = "metaTab",
        name  = "metaTab",
        br(),
        downloadButton("saveMetaBtn", "Download plate summary"),
        br(), br(),
        h4(id = "aggregateDesc",
          "Select multiple rows to see combined statistics",
          helpPopup(paste("Selecting multiple rows by clicking on them will show a table at the",
                          "bottom of the page. The table will show statistics for all the selected wells combined."))
        ),
        DT::dataTableOutput("metaTable"),
        DT::dataTableOutput("metaAggregate", width = 500)
      ),
      
      # Plot tab ----
      tabPanel(
        title = "Plot",
        id = "plotTab",   
        
        br(),
        div(
          id = "plotOptionsSection",
          tabsetPanel(
            id = "plotParamsTabs", type = "pills",    
            
            tabPanel(
              title = "General options",
              id = "plotGeneralTab",
              br(),
              fixedRow(
                column(
                  4,
                  selectInput(
                    "plotParamSubsetType", "Wells to plot",
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
                  div(
                    `data-ddpcr-type` = paste(plate_types$hex_positive_pnpp, "ddpcr_plate", plate_types$fam_positive_pnpp, collapse = " "),
                    checkboxInput("plotParam_show_failed_wells", "Include failed wells", TRUE)
                  ),
                  checkboxInput("plotParam_show_drops", "Show droplets", TRUE)
                ),
                column(
                  4,
                  numericInput("plotParam_drops_size", "Droplets size", 2, 0, 50),
                  selectInput("plotParam_col_drops", "Droplets colour",
                              allCols, "black"),
                  sliderInput("plotParam_alpha_drops", "Droplets transparency",
                              0, 1, 0.1, 0.05, ticks = FALSE)
                ),
                column(
                  4,
                  checkboxInput("plotParam_superimpose", "Superimpose all data in one panel", FALSE),
                  checkboxInput("plotParam_show_full_plate", "Show full plate", FALSE),
                  div(
                    `data-ddpcr-type` = plate_types$custom_thresholds,
                    checkboxInput("plotParam_show_thresholds", "Show threshold borders", TRUE),
                    selectInput("plotParam_col_thresholds", "Threshold borders colour",
                                allCols, "black")
                  ),
                  div(
                    `data-ddpcr-type` = paste(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp, collapse = " "),
                    checkboxInput("plotParam_show_mutant_freq", "Show mutant frequency", TRUE),
                    numericInput("plotParam_text_size_mutant_freq", "Mutant frequency text size", 4, 0, 100)
                  )
                )
              )
            ),
            
            tabPanel(
              title = "Droplets",
              id = "plotDropsTab",
              conditionalPanel(
                "!input.plotParam_show_drops",
                br(),
                h4("Turn on \"Show droplets\" in the General options",
                   "to see more droplet options."),
                br()
              ),
              conditionalPanel(
                "input.plotParam_show_drops",
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
                    `data-ddpcr-type` = paste(plotDropsParams[[x]]$type, collapse = " "),
                    fixedRow(
                      column(
                        3,
                        tags$label(strong(HTML(plotDropsParams[[x]]$name))),
                        class = "plotParamDropName"
                      ),
                      column(
                        3,
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
              title = "Figure options",
              id = "plotFigureTab",
              br(),
              fixedRow(
                column(
                  4,
                  textInput("plotParam_title", "Title", NULL),
                  textInput("plotParam_xlab", "X-axis label", ""),
                  textInput("plotParam_ylab", "Y-axis label", "")
                ),
                column(
                  4,
                  numericInput("plotParam_text_size_title", "Title text size", 14, 0, 100),
                  numericInput("plotParam_text_size_axes_labels", "X/Y labels text size", 12, 0, 100),
                  numericInput("plotParam_text_size_grid_labels", "Grid line labels text size", 12, 0, 100),
                  numericInput("plotParam_text_size_row_col", "Row/column number text size", 12, 0, 100)
                ),
                column(
                  4,
                  selectInput("plotParam_height_type", "Height",
                              c("Automatic" = "auto", "Custom (specify pixels)" = "custom")),
                  conditionalPanel(
                    "input.plotParam_height_type == 'custom'",
                    numericInput("plotParam_height", NULL, 500, min = 0, step = 50)
                  ),
                  selectInput("plotParam_width_type", "Width",
                              c("Automatic" = "auto", "Custom (specify pixels)" = "custom")),
                  conditionalPanel(
                    "input.plotParam_width_type == 'custom'",
                    numericInput("plotParam_width", NULL, 500, min = 0, step = 50)
                  ),
                  checkboxInput("plotParam_show_grid", "Show grid lines", FALSE),
                  checkboxInput("plotParam_show_grid_labels", "Label grid lines", FALSE)
                )              
              )
            ),
            tabPanel(
              title = "Well colours",
              id = "plotWellTab",
              br(),
              fixedRow(
                column(
                  6,
                  selectInput("plotParam_bg_unused", "Unused wells colour",
                              allCols, "white"),
                  selectInput("plotParam_bg_failed", "Failed wells colour",
                              allCols, "gray7"),
                  sliderInput("plotParam_alpha_bg_failed", "Transparency of failed wells",
                              0, 1, 0.7, 0.05, ticks = FALSE)
                ),
                column(
                  6,
                  div(
                    `data-ddpcr-type` = paste(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp, collapse = " "),
                    checkboxInput("plotParam_show_low_high_mut_freq",
                                  "Different colours for wells with high vs low mutant frequency",
                                  TRUE),
                    selectInput("plotParam_bg_mutant", "Mutant wells colour",
                                allCols, "purple3"),
                    selectInput("plotParam_bg_wildtype", "Wild type wells colour",
                                allCols, "green3"),
                    sliderInput("plotParam_alpha_bg_low_high_mut_freq",
                                "Transparency of mutant/wild type wells",
                                0, 1, 0.1, 0.05, ticks = FALSE)
                  )
                )
              )
            )
          )
        ),
        actionButton(
          "plotBtn",
          "Plot",
          class = "btn-primary btn-lg"
        ),
        hidden(downloadButton("downloadPlot", "Save figure")),
        hidden(
          div(id = "mainPlotContainer",
              img(src = "ajax-loader.gif", id = "plotSpinner"),
              plotOutput("mainPlot", width = "auto", height = "auto")
          )
        )
      ),
      
      # Explore summary variable tab ----
      tabPanel(
        title = "Explore summary variable",
        id    = "exploreTab",
        br(),
        uiOutput("exploreVarOutput"),
        selectInput("explorePlotType", "Graph type",
                    c("Box plot" = "box",
                      "Density plot" = "density",
                      "Histogram" = "histogram")),
        downloadButton("saveExplorePlot", "Save figure"),
        plotOutput("explorePlot", width = "500")
      ),

      # Droplets data tab ----
      tabPanel(
        title = "Droplets data",
        id = "dropletsTab",
        br(),
        downloadButton("saveDropletsBtn", "Download droplets data"),
        br(), br(),
        DT::dataTableOutput("dropletsTable", width = 500),
        div(id = "clustersMappingOuter",
            span(id = "clustersMappingInner",
                 div(id = "clustersTitle", "Clusters"),
                 uiOutput("clustersMapping")
            )
        )        
      )
    )
  )
)