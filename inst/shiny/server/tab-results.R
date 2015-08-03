# ddPCR R package - Dean Attali 2015
# --- Results tab server --- #

# return whether or not the current dataset has sample names
hasSampleNames <- eventReactive(dataValues$plate, {
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  return(any(!is.na(meta[['sample']])))
})

# return the indices of plate meta columns that correspond to variables
# that aren't important enough to show by default
metaColsHideIdx <- eventReactive(dataValues$plate, {
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  colsHide <- c("row", "col", "used", "mutant_border", "filled_border")
  if (!hasSampleNames()) {
    colsHide <- c(colsHide, "sample")
  }
  which(colnames(meta) %in% colsHide) - 1
})

# return all numeric metadata variables except for column
metaNumericVars <- eventReactive(dataValues$plate, {
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  vars <- vapply(meta, is.numeric, logical(1)) %>% which %>% names
  vars <- vars[vars != "col"]
})

# Update the plot parameters whenever the plate gets updated
observeEvent(dataValues$plate, {
  updateTextInput(session, "plotParam_xlab", value = dataValues$plate %>% x_var)
  updateTextInput(session, "plotParam_ylab", value = dataValues$plate %>% y_var)
  
  # hide/show the droplet options only for droplets that exist in this plate type
  hide(selector = "[data-ddpcr-type]")
  show(selector = sprintf("[data-ddpcr-type~=%s]", dataValues$plate %>% type))
  if (type(dataValues$plate) == CROSSHAIR_THRESHOLDS) {
    updateSelectInput(session, "plotParamDropShow-empty", selected = "TRUE")
  }  
})

# Droplets data tab ----

# show the droplets data table
output$dropletsTable <- DT::renderDataTable(
  dataValues$plate %>% plate_data,
  rownames = FALSE,
  selection = "none",
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

# show the cluster number --> cluster name mapping
output$clustersMapping <- renderUI({
  lapply(seq_along(clusters(dataValues$plate)), function(x) {
    cluster_name <- tolower(cluster_name(dataValues$plate, x))
    if (inherits(dataValues$plate, PPNP_ASSAY)) {
      cluster_name <- meta_var_name(dataValues$plate, cluster_name)
    }
    div(x, "=", cluster_name)
  })
})

# Plate summary tab ----

# show plate summary table
output$metaTable <- DT::renderDataTable({
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  colnames <- meta %>% colnames %>% humanFriendlyNames
  DT::datatable(meta,
                rownames = FALSE,
                class = 'cell-border stripe',
                colnames = colnames,
                extensions = c("ColVis"),
                options = list(
                  searching = FALSE, paging = FALSE,
                  scrollX = TRUE, scrollY = 500,
                  columnDefs = list(list(visible = FALSE,
                                         targets = metaColsHideIdx())),
                  dom = 'C<"clear">lfrtp',
                  scrollCollapse = TRUE
                )
  )
})

# show statistics for selected wells
output$metaAggregate <- DT::renderDataTable({
  if (is.null(input$metaTable_rows_selected)) {
    return()
  }
  
  wells <- input$metaTable_rows_selected
  vars <- metaNumericVars()
  niceVars <- humanFriendlyNames(vars)
  selectInput("exploreVarSelect", "Choose summary variable",
              setNames(vars, niceVars))
  
  meta <-
    dataValues$plate %>%
    subset(wells) %>%
    plate_meta(only_used = TRUE) %>%
    dplyr::select_(~ one_of(vars)) %>%
    magrittr::set_colnames(humanFriendlyNames(colnames(.)))
  
  # calculate mean and standard error for each numeric variable
  data <- 
    plyr::ldply(meta, function(x) {
      data.frame(Mean = mean(x, na.rm = TRUE),
                 `Standard error` = sd(x, na.rm = TRUE) / sqrt(length(x)),
                 check.names = FALSE)},
      .id = "Variable"
    )
  data[] <- lapply(data, format, scientific = FALSE, big.mark = ",", drop0trailing = TRUE) 

  DT::datatable(data,
                rownames = FALSE,
                class = 'cell-border stripe',
                selection = "none",
                options = list(
                  searching = FALSE, paging = FALSE, scrollCollapse = TRUE,
                  info = FALSE, ordering = FALSE
                )
  )
})

# download plate summary 
output$saveMetaBtn <- downloadHandler(
  filename = function() { 
    sprintf("%s-summary.csv", dataValues$plate %>% name)
  },
  content = function(file) {
    write.csv(dataValues$plate %>% plate_meta, file, row.names = FALSE)
  }
)

# Explore variable tab ----

# Show a select input with all numeric variables as options
output$exploreVarOutput <- renderUI({
  vars <- metaNumericVars()
  niceVars <- humanFriendlyNames(vars)
  selectInput("exploreVarSelect", "Choose summary variable",
              setNames(vars, niceVars))
})

# make the exploratory plot for the selected variable
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

# render exploratory plot
output$explorePlot <- renderPlot({
  makeExplorePlot()
})

# save exploratory plot
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

# Plot tab ----

# keep track of the last plot so we can easily download it
dataValues$lastPlot <- NULL

# when plot button is clicked, show the download button and the plot
observeEvent(input$plotBtn, {
  show("mainPlotContainer")
  show("downloadPlot")
})

# download plot
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

# calculate height of plot
calcPlotHeight <- eventReactive(makePlot(), {
  # if user specified custom height, use that
  if (input$plotParam_height_type == "custom") {
    return(input$plotParam_height)
  }
  
  # calculate height based on number of rows and the plot parameters
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
  
  # update the custom height input so that the user can see what height was used
  updateNumericInput(session, "plotParam_height", value = height)
  
  height
})

# calculate width of plot to be used in the app UI
calcPlotWidth <- eventReactive(makePlot(), {
  # if user specified width, use that; otherwise use "auto" to adjust the
  # width automatically based on the height while staying within the bounds
  if (input$plotParam_width_type == "custom") {
    return(input$plotParam_width)
  } else {
    calcPlotWidthForce() # call this only so that the width input will be updated
    return("auto")
  }
})

# calculate width of plot to be used when downloading image
calcPlotWidthForce <- eventReactive(makePlot(), {
  if (input$plotParam_width_type == "custom") {
    return(input$plotParam_width)
  }
  
  # calculate width based on number of columns and the plot parameters
  plot <- makePlot()
  cols <- attr(plot, 'ddpcr_cols')
  size <- ifelse(cols > 8, 70, 100)
  width <- 
    (cols * size) +
    (nzchar(input$plotParam_ylab) * input$plotParam_text_size_axes_labels) +
    (isTRUE(input$plotParam_show_grid_labels) * 2 * input$plotParam_text_size_grid_labels) +
    input$plotParam_text_size_row_col +
    100
  
  # update the custom width input so that the user can see what width was used
  updateNumericInput(session, "plotParam_width", value = width)
  
  width
})

# generate the plot object when the plot button is clicked
makePlot <- eventReactive(input$plotBtn, {
  withBusyIndicator("plotBtn", {
    plotParams <- list()
    plotParams[['x']] <- dataValues$plate
    
    # gather all general settings
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
    
    # gather all droplet settings
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
    
    # gather all figure settings
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
    
    # gather all well colour settings
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
    
    # now we have all the plot settings, create the plot and save it
    plot <- do.call(plot, plotParams)
    dataValues$lastPlot <- plot
    
    plot
  })
})

# render the main plot
output$mainPlot <- renderPlot(
  makePlot(),
  width = function() { calcPlotWidth() },
  height = function() { calcPlotHeight() },
  units = "px",
  res = 100
)

# logic that turns certain plot options on/off if they conflict with other options
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

# when the main transparency for drops changes, update all individual drops 
observeEvent(input$plotParam_alpha_drops, {
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
    tags$i("Cannot filter by sample names since this dataset doesn't have sample name information")
  }
})