# ddPCR R package - Dean Attali 2015
# --- Settings tab server --- #

# Update the settings whenever the plate gets updated
observeEvent(dataValues$plate, {
  updateSelectInput(session, "settingsPlateType", selected = dataValues$plate %>% type)
  updateTextInput(session, "settingsName", value = dataValues$plate %>% name)
  updateTextInput(session, "settingsXvar", value = dataValues$plate %>% x_var)
  updateTextInput(session, "settingsYvar", value = dataValues$plate %>% y_var)
  if (type(dataValues$plate) == plate_types$custom_thresholds) {
    updateTextInput(session, "settingsXThreshold", value = dataValues$plate %>% x_threshold)
    updateTextInput(session, "settingsYThreshold", value = dataValues$plate %>% y_threshold)
  }
  updateTextInput(session, "settingsSubset", value = "")
})

# Basic settings ----

# update basic settings button is clicked
observeEvent(input$updateBasicSettings, {
  withBusyIndicator("updateBasicSettings", {
    
    # if a new plate type is chosen, need to reset the plate
    if (type(dataValues$plate) != input$settingsPlateType &&
        input$settingsPlateType != "") {
      dataValues$plate <-
        ddpcr::reset(dataValues$plate, input$settingsPlateType)
    }
    
    name(dataValues$plate) <- input$settingsName
    x_var(dataValues$plate) <- input$settingsXvar
    y_var(dataValues$plate) <- input$settingsYvar
    
    if (type(dataValues$plate) == plate_types$custom_thresholds) {
      x_threshold(dataValues$plate) <- input$settingsXThreshold
      y_threshold(dataValues$plate) <- input$settingsYThreshold
    }
  })
})

# Subset plate ----

# subset plate button is clicked
observeEvent(input$updateSubsetSettings, {
  withBusyIndicator("updateSubsetSettings", {
    dataValues$plate <- subset(dataValues$plate, input$settingsSubset)
    updateTextInput(session, "settingsSubset", value = "")
  })
})   

# update the plot that shows what wells are available
output$wellsUsedPlot <- renderPlot({
  meta <- plate_meta(dataValues$plate)
  meta[['col']] <- as.factor(meta[['col']])
  meta[['row']] <- as.factor(meta[['row']])
  meta[['row']] <- factor(meta[['row']], levels = rev(levels(meta[['row']])))
  
  p <-
    ggplot2::ggplot(meta, ggplot2::aes(col, row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = used), color = "#222222", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#333333", "TRUE" = "white")) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      line             = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(size = 20, color = "black"),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::xlab(NULL) + 
    ggplot2::ylab(NULL) +
    ggplot2::coord_fixed()
  p
})  

# user double clicks on a well in the subset plot
observeEvent(input$wellsUsedPlotClick, {
  # find out what well was clicked
  col <- floor(input$wellsUsedPlotClick$x + 0.5) %>% num_to_col
  row <- ceiling(8.5 - input$wellsUsedPlotClick$y) %>% num_to_row
  clickedWell <- sprintf("%s%s", row, col)
  
  # don't do anything if the well isn't available in the data
  if (!clickedWell %in% (dataValues$plate %>% wells_used)) {
    return(NULL)
  }
  
  if (input$settingsSubset == "") {
    newValue <- clickedWell
  } else {
    newValue <- sprintf("%s, %s", input$settingsSubset, clickedWell)
  }
  updateTextInput(session, "settingsSubset", value = newValue)
})

# user selects a region in the subset plot
observeEvent(input$wellsUsedPlotBrush, {
  # figure out what wells are selected
  col1 <- floor(input$wellsUsedPlotBrush$xmin + 0.5) %>% num_to_col
  col2 <- floor(input$wellsUsedPlotBrush$xmax + 0.5) %>% num_to_col
  row1 <- ceiling(8.5 - input$wellsUsedPlotBrush$ymin) %>% num_to_row
  row2 <- ceiling(8.5 - input$wellsUsedPlotBrush$ymax) %>% num_to_row
  
  well1 <- sprintf("%s%s", row1, col1)
  well2 <- sprintf("%s%s", row2, col2)
  
  # if none of the wells are available in the data, ignore
  if (length(well1) == 0 || length(well2) == 0 ||
        !grepl(WELL_ID_REGEX, well1) || !grepl(WELL_ID_REGEX, well2) ||
        !any((dataValues$plate %>% wells_used) %in% get_wells_btwn(well1, well2))) {
    return(NULL)
  }
  
  if (input$settingsSubset == "") {
    newValue <- sprintf("%s:%s", well1, well2)
  } else {
    newValue <- sprintf("%s, %s:%s", input$settingsSubset, well1, well2)
  }    
  updateTextInput(session, "settingsSubset", value = newValue)
})

# Advanced settings ----

# When the advanced settings update button is clicked,
# check all advanced settings and save them
observeEvent(input$updateAdvancedSettings, {
  withBusyIndicator("resetParamsBtn", {
    disable("updateAdvancedSettings")
    advanced_param_regex <- "^advanced_setting_param_(.*)__(.*)$"
    all_params <- 
      grep(advanced_param_regex, names(input), value = TRUE)
    lapply(all_params, function(x) {
      if (!is.null(input[[x]]) && !is.na(input[[x]])) {
        major_name <- gsub(advanced_param_regex, "\\1", x)
        minor_name <- gsub(advanced_param_regex, "\\2", x)
        params(dataValues$plate, major_name, minor_name) <- input[[x]]
      }
    })
    enable("updateAdvancedSettings")
  })
})

# reset settings to default
observeEvent(input$resetParamsBtn, {
  withBusyIndicator("resetParamsBtn", {
    disable("updateAdvancedSettings")
    dataValues$plate <- set_default_params(dataValues$plate)
    enable("updateAdvancedSettings")
  })
})

# When the plate changes, update the advanced settings UI
observeEvent(dataValues$plate, {
  plate <- dataValues$plate
  output$advancedSettings <- renderUI({
    
    # loop through the parameters and create an input for each one
    lapply(
      plate %>% params %>% names,
      function(major_name) {
        lapply(
          plate %>% params %>% .[[major_name]] %>% names,
          function(minor_name) {
            param_name <- sprintf("%s::%s", major_name, minor_name)
            
            params_ignore <- c(
              "GENERAL::X_VAR", "GENERAL::Y_VAR",
              "CLASSIFY::X_THRESHOLD", "CLASSIFY::Y_THRESHOLD"
            )
            
            if (param_name %in% params_ignore) {
              return(NULL)
            }
            
            param_val <- plate %>% params %>% .[[c(major_name, minor_name)]] 
            param_id <- sprintf("advanced_setting_param_%s__%s", major_name, minor_name)
            
            
            # in order to ensure the correct type for each variable,
            # we need to make sure that boolean/numeric/string parameters
            # are rendered in an appropriate input type. Otherwise if I used
            # textInput for all of them, then all parameters would be
            # converted to string when reading them back.
            if (param_val %>% is.logical) {
              input_type <- checkboxInput
            } else if (param_val %>% is.numeric) {
              input_type <- numericInput
            } else {
              input_type <- textInput
            }
            do.call(input_type, list(param_id, param_name, param_val))
          }
        )
      }
    )
  })
})

# change to analyze tab when clicking on link
observeEvent(input$toAnalyze,
  updateTabsetPanel(session, "mainNav", "analyzeTab")
)