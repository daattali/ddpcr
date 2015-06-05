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
  colnames <- meta %>% colnames %>% humanFriendlyNames
  DT::datatable(meta,
    rownames = FALSE,
    class = 'cell-border stripe',
    colnames = colnames,
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

humanFriendlyNames <- function(colnames) {
  paste0(toupper(substring(colnames, 1, 1)),
         substring(gsub("_", " ", colnames), 2))
}

# --- Plot tab --- #

dataValues$lastPlot <- NULL

observeEvent(input$plotBtn, {
  show("mainPlotContainer")
  show("downloadPlot")
})

output$downloadPlot <- downloadHandler(
  filename = function() { 
    sprintf("%s-plot.png", dataValues$plate %>% name)
  },
  content = function(file) {
    png(file,
        width = calcPlotWidthForce(),
        height = calcPlotHeight(),
        units = "px",
        res = 100
    )
    print(dataValues$lastPlot)
    dev.off()
  }
)

# calculate height of plot (or use a user-entered value)
calcPlotHeight <- eventReactive(makePlot(), {
  if (input$plotParam_height_type == "custom") {
    return(input$plotParam_height)
  }
  
  plot <- makePlot()
  rows <- attr(plot, 'ddpcr_rows')
  cols <- attr(plot, 'ddpcr_cols')
  size <- ifelse(cols > 8, 70, 100)
  height <- 
    (rows * size) +
    (nzchar(input$plotParam_title) * input$plotParam_text_size_title) +
    (nzchar(input$plotParam_xlab) * input$plotParam_text_size_axes_labels) +
    (isTRUE(input$plotParam_show_grid_labels) * 2 * input$plotParam_text_size_grid_labels) +
    input$plotParam_text_size_row_col + 
    100
  
  updateNumericInput(session, "plotParam_height", value = height)
  
  height
})

calcPlotWidthForce <- eventReactive(makePlot(), {
  if (input$plotParam_width_type == "custom") {
    return(input$plotParam_width)
  }
  
  plot <- makePlot()
  cols <- attr(plot, 'ddpcr_cols')
  size <- ifelse(cols > 8, 70, 100)
  width <- 
    (cols * size) +
    (nzchar(input$plotParam_ylab) * input$plotParam_text_size_axes_labels) +
    (isTRUE(input$plotParam_show_grid_labels) * 2 * input$plotParam_text_size_grid_labels) +
    input$plotParam_text_size_row_col +
    100
  
  updateNumericInput(session, "plotParam_width", value = width)
  width
})

calcPlotWidth <- eventReactive(makePlot(), {
  if (input$plotParam_width_type == "custom") {
    return(input$plotParam_width)
  } else {
    calcPlotWidthForce()
    return("auto")
  }
})

makePlot <- eventReactive(input$plotBtn, {
  disable("plotBtn")
  on.exit({
    enable("plotBtn")
  })
  hide("errorDiv")  

  tryCatch({
    plotParams <- list()
    plotParams[['x']] <- dataValues$plate
    
    # general settings
    if (input$plotParamSubsetType == 'wells' && !is.null(input$plotParamWells)) {
      plotParams[['wells']] <- input$plotParamWells
    } else if (input$plotParamSubsetType == 'samples' && !is.null(input$plotParamSamples)) {
      plotParams[['samples']] <- input$plotParamSamples
    }
    generalParamNames <-
      c("show_failed_wells", "show_drops", "drops_size", "col_drops", "alpha_drops",
        "superimpose", "show_full_plate", "show_thresholds", "col_thresholds",
        "show_mutant_freq", "text_size_mutant_freq")
    generalParams <-
      lapply(generalParamNames, function(x) {
        inputName <- sprintf("plotParam_%s", x)
        value <- input[[inputName]]
        if (is.na(value)) {
          err_msg(sprintf("Invalid value for %s", x))
        }
        setNames(value, x) %>% as.list
      })
    generalParams <- unlist(generalParams, recursive = FALSE)
    plotParams <- append(plotParams, generalParams)    
    
    # droplet settings
    if (input$plotParam_show_drops) {
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
            paramList[[paramNameCol]] <- input$plotParam_col_drops
          }
          paramList
        })
      dropsParams <- unlist(dropsParams, recursive = FALSE)
      plotParams <- append(plotParams, dropsParams)
    }
  
    # figure settings
    if (nzchar(input$plotParam_title)) {
      plotParams[['title']] <- input$plotParam_title
    } else {
      plotParams['title'] <- list(NULL) # note the trick with one bracket to assign NULL
    }
    if (nzchar(input$plotParam_xlab)) {
      plotParams[['xlab']] <- input$plotParam_xlab
    } else {
      plotParams['xlab'] <- list(NULL)
    }
    if (nzchar(input$plotParam_ylab)) {
      plotParams[['ylab']] <- input$plotParam_ylab
    } else {
      plotParams['ylab'] <- list(NULL)
    }
    figureParamNames <-
      c("show_grid", "show_grid_labels",
        "text_size_title", "text_size_axes_labels",
        "text_size_grid_labels", "text_size_row_col")
    figureParams <-
      lapply(figureParamNames, function(x) {
        inputName <- sprintf("plotParam_%s", x)
        value <- input[[inputName]]
        if (is.na(value)) {
          err_msg(sprintf("Invalid value for %s", x))
        }
        setNames(value, x) %>% as.list
      })
    figureParams <- unlist(figureParams, recursive = FALSE)
    plotParams <- append(plotParams, figureParams)
    
    # well colour settings
    wellParamNames <-
      c("bg_unused", "bg_failed", "alpha_bg_failed",
        "show_low_high_mut_freq", "bg_mutant", "bg_wildtype",
        "alpha_bg_low_high_mut_freq")
    wellParams <-
      lapply(wellParamNames, function(x) {
        inputName <- sprintf("plotParam_%s", x)
        value <- input[[inputName]]
        if (is.na(value)) {
          err_msg(sprintf("Invalid value for %s", x))
        }
        setNames(value, x) %>% as.list
      })
    wellParams <- unlist(wellParams, recursive = FALSE)
    plotParams <- append(plotParams, wellParams)
        
    plot <- do.call(plot, plotParams)
    dataValues$lastPlot <- plot
    plot
  }, error = errorFunc)
})

# main plot
output$mainPlot <- renderPlot(
  makePlot(),
  width = function() { calcPlotWidth() },
  height = function() { calcPlotHeight() },
  units = "px",
  res = 100
)

# logic that turns certain options on/off if they conflict with other options
observe({
  toggleState("plotParam_drops_size", input$plotParam_show_drops)
  toggleState("plotParam_col_drops", input$plotParam_show_drops)
  toggleState("plotParam_alpha_drops", input$plotParam_show_drops)
  toggleState("plotParam_superimpose", !input$plotParam_show_full_plate && input$plotParam_show_drops)
  toggleState("plotParam_show_full_plate", !input$plotParam_superimpose)
  toggleState("plotParam_text_size_mutant_freq", input$plotParam_show_mutant_freq)
  toggleState("plotParam_col_thresholds", input$plotParam_show_thresholds)
  toggleState("plotParam_bg_mutant", input$plotParam_show_low_high_mut_freq)
  toggleState("plotParam_bg_wildtype", input$plotParam_show_low_high_mut_freq)
  toggleState("plotParam_alpha_bg_low_high_mut_freq", input$plotParam_show_low_high_mut_freq)
})

observeEvent(input$plotParam_show_failed_wells, {
  updateSelectInput(session, "plotParamDropShow-failed",
                    selected = as.character(input$plotParam_show_failed_wells))
  toggleState("plotParam_bg_failed", input$plotParam_show_failed_wells)
  toggleState("plotParam_alpha_bg_failed", input$plotParam_show_failed_wells)
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
  value <- input$plotParam_alpha_drops
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

# --- Explore tab ---

# Show a select input with all numeric variables as options
output$exploreVarOutput <- renderUI({
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  vars <- vapply(meta, is.numeric, logical(1)) %>% which %>% names
  vars <- vars[vars != "col"]
  niceVars <- humanFriendlyNames(vars)
  selectInput("exploreVarSelect", "Choose summary variable",
              setNames(vars, niceVars))
})

# this is a function rather than a reactive because base graphics
# don't play nice with reactives because they don't store the plot
# in the return value so it's impossible to reuse the value
makeExplorePlot <- function() {
  if (is.null(input$exploreVarSelect)) {
    return()
  }
  
  data <- dataValues$plate %>% plate_meta %>% .[[input$exploreVarSelect]]
  niceVar <- humanFriendlyNames(input$exploreVarSelect)
  title <- sprintf("%s per well", niceVar)
  if (input$explorePlotType == "box") {
    boxplot(data, main = title, ylab = niceVar, col = "#eeeeee")
  } else if (input$explorePlotType == "density") {
    dens <- density(data, na.rm = TRUE)
    plot(dens, main = title, xlab = niceVar, ylab = "")
    polygon(dens, col = "#eeeeee")
  } else {
    hist(data, col = "#eeeeee", main = title, xlab = niceVar, ylab = "# of wells")
  }
}

output$explorePlot <- renderPlot({
  makeExplorePlot()
})

output$saveExplorePlot <- downloadHandler(
  filename = function() { 
    sprintf("%s-%s.png", dataValues$plate %>% name, input$exploreVarSelect)
  },
  content = function(file) {
    png(file,
        width = 500,
        height = 400,
        units = "px",
        res = 100
    )
    print(makeExplorePlot())
    dev.off()
  }
)

