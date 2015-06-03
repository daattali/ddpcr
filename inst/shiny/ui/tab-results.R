tabPanel(
  title = "Results",
  id    = "resultsTab",
  value = "resultsTab",
  name  = "resultsTab",
  
  conditionalPanel(
    condition = "output.datasetChosen",
  
    tabsetPanel(
      id = "resultsTabs", type = "tabs",    
      
      # Plot tab
      tabPanel(
        title = "Plot",
        id = "plotTab",   

        br(),
        div("Settings",
        tabsetPanel(
          id = "plotParamsTabs", type = "pills",    
          
          tabPanel(
            title = "General",
            id = "plotTab"
          ),
          tabPanel(
            title = "Droplet colours",
            id = "plotTab"
          ),
          tabPanel(
            title = "Background colours",
            id = "plotTab"
          )
        ))
      
        
#         sidebarLayout(
#           sidebarPanel(
#             h3("Plot parameters"),
#             lapply(
#               formals(ddpcr:::plot.ddpcr_plate) %>% names,
#               function(param_name) {
#                 param_val <- formals(ddpcr:::plot.ddpcr_plate) %>% .[[param_name]]
#                 param_id <- sprintf("plot_param_%s", param_name)
#                 if (missing(param_val) || param_val %>% is.language) {
#                   return()
#                 } else if (param_val %>% is.logical) {
#                   input_type <- checkboxInput
#                 } else if (param_val %>% is.numeric) {
#                   input_type <- numericInput
#                 } else {
#                   input_type <- textInput
#                 }
#                 do.call(input_type, list(param_id, param_name, param_val))
#               }
#             )
#           ),
#           
#           mainPanel(
#             actionButton(
#               "plotBtn",
#               "Plot",
#               class = "btn-primary"
#             ),
#             plotOutput("plot")
#           )
#         )
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
      
      # Plate data tab
      tabPanel(
        title = "Plate summary",
        id = "metaTab",
        br(),
        downloadButton("saveMetaBtn", "Download plate summary"),
        br(), br(),
        DT::dataTableOutput("metaTable")
      )      
    )
  )
)
