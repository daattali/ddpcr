# --- Droplets data tab ---

output$dropletsTable <- DT::renderDataTable(
  dataValues$plate %>% plate_data,
  rownames = FALSE,
  options = list(searching = FALSE)
)

# download droplets data
output$saveDropletsBtn <- downloadHandler(
  filename = function() { 
    sprintf("%s-droplets.csv", dataValues$plate %>% name)
  },
  content = function(file) {
    write.csv(dataValues$plate %>% plate_data, file, row.names = FALSE)
  }
)

output$clustersMapping <- renderUI({
  lapply(seq_along(clusters(dataValues$plate)), function(x) {
    div(x, "=", tolower(cluster_name(dataValues$plate, x)))
  })
})

# --- Plate summary tab ---

output$metaTable <- DT::renderDataTable({
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  
  DT::datatable(meta,
    rownames = FALSE,
    class = 'cell-border stripe',
    colnames = humanFriendlyColname(),
    extensions = list(
      'ColVis' = NULL,  # show the "show/hide columns" button 
      FixedColumns = list(leftColumns = 1)  # fix the Well column
    ),
    options = list(
      searching = FALSE, paging = FALSE,
      scrollX = TRUE, scrollY = 500,
      columnDefs = list(list(visible = FALSE,
                             targets = metaColsHideIdx())),
      dom = 'C<"clear">lfrtip',
      scrollCollapse = TRUE
    )
  )
})

output$saveMetaBtn <- downloadHandler(
  filename = function() { 
    sprintf("%s-summary.csv", dataValues$plate %>% name)
  },
  content = function(file) {
    write.csv(dataValues$plate %>% plate_meta, file, row.names = FALSE)
  }
)

hasSampleNames <- function() {
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  return(any(!is.na(meta[['sample']])))
}

metaColsHideIdx <- function() {
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  colsHide <- c("row", "col", "used", "comment",
                "mutant_borders", "wildtype_borders", "filled_borders")
  if (!hasSampleNames()) {
    colsHide <- c(colsHide, "sample")
  }
  which(colnames(meta) %in% colsHide) - 1
}

humanFriendlyColname <- function() {
  colnames <- dataValues$plate %>% plate_meta(only_used = TRUE) %>% colnames
  paste0(toupper(substring(colnames, 1, 1)),
         substring(gsub("_", " ", colnames), 2))
}

# --- Plot tab --- #

output$downloadPlot <- downloadHandler(
  filename = function() { 
    sprintf("%s-plot.png", dataValues$plate %>% name)
  },
  content = function(file) {
    png(file
        #width = plotWidth(),
        #height = plotHeight(),
        #units = "px",
        #res = 100
    )
    print(plotInput()$p)
    dev.off()
  }
)

# plot button is clicked
output$plot <- renderPlot({
  if (input$plotBtn == 0) return()
  
  isolate({
    plot_params <- 
      sapply(formals(ddpcr:::plot.ddpcr_plate) %>% names,
             function(x) input[[sprintf("plot_param_%s", x)]] ) %>%
      .[!lapply(., is.null) %>% unlist]
    plot_params[['x']] <- dataValues$plate
    do.call(plot, plot_params)
  })
}, height = "auto")

# logic that turns certain options on/off if they conflict with other options
observe({
  if (input$plotParamShowDrops) {
    enable("plotParamDropsSize")
    enable("plotParamDropsCol")
    enable("plotParamDropsAlpha")
  } else {
    disable("plotParamDropsSize")
    disable("plotParamDropsCol")
    disable("plotParamDropsAlpha")
  }
  
  toggleState("plotParamSuperimpose", !input$plotParamShowFullPlate && input$plotParamShowDrops)
  toggleState("plotParamShowFullPlate", !input$plotParamSuperimpose)
  toggleState("plotParamMutFreqSize", input$plotParamShowFreq)
})

# create select box input for choosing wells and sample
output$plotParamWellsSelect <- renderUI({
  selectizeInput("plotParamWells", NULL,
                 dataValues$plate %>% wells_used,
                 selected = NULL, multiple = TRUE,
                 options = list(placeholder = "Select wells"))
})
output$plotParamSamplesSelect <- renderUI({
  if (hasSampleNames()) {
    selectizeInput("plotParamSamples", NULL,
                   well_info(dataValues$plate, dataValues$plate %>% wells_used, "sample"),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select samples"))
  } else {
    "There are no sample names in this dataset"
  }
})