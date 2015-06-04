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

observeEvent(input$plotBtn, {
  show("mainPlotContainer")
  show("downloadPlot")
})

makePlot <- eventReactive(input$plotBtn, {
  plotParams <- list()
  plotParams[['x']] <- dataValues$plate
  
  # general settings
  if (input$plotParamSubsetType == 'wells' && !is.null(input$plotParamWells)) {
    plotParams[['wells']] <- input$plotParamWells
  } else if (input$plotParamSubsetType == 'samples' && !is.null(input$plotParamSamples)) {
    plotParams[['samples']] <- input$plotParamSamples
  }
  plotParams[['show_failed_wells']] <- input$plotParamIncludeFailed
  plotParams[['show_drops']] <- input$plotParamShowDrops
  plotParams[['drops_size']] <- input$plotParamDropsSize
  plotParams[['col_drops']] <- input$plotParamDropsCol
  plotParams[['alpha_drops']] <- input$plotParamDropsAlpha
  plotParams[['superimpose']] <- input$plotParamSuperimpose
  plotParams[['show_full_plate']] <- input$plotParamShowFullPlate
  if (!is.null(input$plotParamShowThresholds)) {
    plotParams[['show_thresholds']] <- input$plotParamShowThresholds
  }
  if (!is.null(input$plotParamThresholdsCol)) {
    plotParams[['col_thresholds']] <- input$plotParamThresholdsCol
  }
  if (!is.null(input$plotParamShowFreq)) {
    plotParams[['show_mutant_freq']] <- input$plotParamShowFreq
  }
  if (!is.null(input$plotParamMutFreqSize)) {
    plotParams[['text_size_mutant_freq']] <- input$plotParamMutFreqSize
  }
  
  # droplet settings
  if (input$plotParamShowDrops) {
    dropsParams <- 
      lapply(dataValues$plate %>% clusters %>% tolower, function(x) {
        inputNameShow <- sprintf("plotParamDropShow-%s", x)
        inputNameCol <- sprintf("plotParamDropCol-%s", x)
        inputNameAlpha <- sprintf("plotParamDropAlpha-%s", x)
        paramNameShow <- sprintf("show_drops_%s", x)
        paramNameCol <- sprintf("col_drops_%s", x)
        paramNameAlpha <- sprintf("alpha_drops_%s", x)
        if (is.null(input[[inputNameShow]])) {
          return()
        }
        paramList <- list()
        paramList[[paramNameShow]] <- as.logical(input[[inputNameShow]])
        paramList[[paramNameCol]] <- input[[inputNameCol]]
        paramList[[paramNameAlpha]] <- input[[inputNameAlpha]]
        if (paramList[[paramNameCol]] == "Default") {
          paramList[[paramNameCol]] <- input$plotParamDropsCol
        }
        paramList
      })
    dropsParams <- unlist(dropsParams, recursive = FALSE)
    plotParams <- append(plotParams, dropsParams)
  }
#   plotParams[['']] <- input$
#   plotParams[['']] <- input$
#   plotParams[['']] <- input$
#   plotParams[['']] <- input$
#   plotParams[['']] <- input$
#     
  
  do.call(plot, plotParams)
})

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

# main plot
output$mainPlot <- renderPlot({
  makePlot()
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
  toggleState("plotParamThresholdsCol", input$plotParamShowThresholds)
  toggle(id = "plotParamsDropRow-failed", condition = input$plotParamIncludeFailed)
})

# if the user chooses to not show a cluster of drops, disable the options
# for that cluster
observe({
  paramsDropShowRegex <- "^plotParamDropShow-(.*)$"
  paramsDropShow <- grep(paramsDropShowRegex, names(input), value = TRUE)
  
  lapply(paramsDropShow, function(x) {
    name <- gsub(paramsDropShowRegex, "\\1", x)

    toggleState(sprintf("plotParamDropCol-%s", name), as.logical(input[[x]]))
    toggleState(sprintf("plotParamDropAlpha-%s", name), as.logical(input[[x]]))
  })
})

observe({
  value <- input$plotParamDropsAlpha
  paramsDropAlphaRegex <- "^plotParamDropAlpha-(.*)$"
  paramsDropAlpha <- grep(paramsDropAlphaRegex, names(input), value = TRUE)
  lapply(paramsDropAlpha, function(x) {
    if (!grepl("outlier", x)) {
      updateSliderInput(session, x, value = value)
    }
  })
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