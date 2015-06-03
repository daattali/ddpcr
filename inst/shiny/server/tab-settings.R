# --- Settings tab --- #

# update the plot that shows what wells are available
output$wellsUsedPlot <- renderPlot({
  meta <- plate_meta(dataValues$plate)
  meta[['col']] <- as.factor(meta[['col']])
  meta[['row']] <- as.factor(meta[['row']])
  meta[['row']] <- factor(meta[['row']], levels = rev(levels(meta[['row']])))
  
  p <-
    ggplot2::ggplot(meta, ggplot2::aes(col, row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = used), color = "#222222", show_guide = FALSE) +
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

# plot that helps select wells is clicked
observeEvent(input$wellsUsedPlotClick, {
  col <- floor(input$wellsUsedPlotClick$x + 0.5) %>% num_to_col
  row <- ceiling(8.5 - input$wellsUsedPlotClick$y) %>% num_to_row
  clickedWell <- sprintf("%s%s", row, col)
  
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

observeEvent(input$wellsUsedPlotBrush, {
  col1 <- floor(input$wellsUsedPlotBrush$xmin + 0.5) %>% num_to_col
  col2 <- floor(input$wellsUsedPlotBrush$xmax + 0.5) %>% num_to_col
  row1 <- ceiling(8.5 - input$wellsUsedPlotBrush$ymin) %>% num_to_row
  row2 <- ceiling(8.5 - input$wellsUsedPlotBrush$ymax) %>% num_to_row
  
  well1 <- sprintf("%s%s", row1, col1)
  well2 <- sprintf("%s%s", row2, col2)
  
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

# update settings button is clicked
observeEvent(input$updateBasicSettings, {
  # User-experience stuff
  disable("updateBasicSettings")
  show("updateBasicSettingsMsg")
  on.exit({
    enable("updateBasicSettings")
    hide("updateBasicSettingsMsg")
  })
  hide("errorDiv")    
  
  tryCatch({
    if (type(dataValues$plate) != input$settingsPlateType &&
          input$settingsPlateType != "") {
      dataValues$plate <- ddpcr::reset(dataValues$plate, input$settingsPlateType)
    }
    
    name(dataValues$plate) <- input$settingsName
    x_var(dataValues$plate) <- input$settingsXvar
    y_var(dataValues$plate) <- input$settingsYvar
    
    if (type(dataValues$plate) == CROSSHAIR_THRESHOLDS) {
      x_threshold(dataValues$plate) <- input$settingsXThreshold
      y_threshold(dataValues$plate) <- input$settingsYThreshold
    }
    
    show("updateBasicSettingsDone")
    hide(id = "updateBasicSettingsDone", anim = TRUE,
         animType = "fade", time = 0.5, delay = 4)
  }, error = errorFunc)
})

observeEvent(input$updateSubsetSettings, {
  # User-experience stuff
  disable("updateSubsetSettings")
  on.exit({
    enable("updateSubsetSettings")
  })
  hide("errorDiv")
  
  tryCatch({
    dataValues$plate <- subset(dataValues$plate, input$settingsSubset)
    updateTextInput(session, "settingsSubset", value = "")
    show("updateSubsetSettingsDone")
    hide(id = "updateSubsetSettingsDone", anim = TRUE,
         animType = "fade", time = 0.5, delay = 4)
  }, error = errorFunc)
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

# When the advanced settings update button is clicked,
# check all advanced settings and save them
observeEvent(input$updateAdvancedSettings, {
  # User-experience stuff
  disable("updateAdvancedSettings")
  on.exit({
    enable("updateAdvancedSettings")
  })
  hide("errorDiv")   
  
  tryCatch({
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
    
    show("updateAdvancedSettingsDone")
    hide(id = "updateAdvancedSettingsDone", anim = TRUE,
         animType = "fade", time = 0.5, delay = 4)
  }, error = errorFunc)
})

observeEvent(input$resetParamsBtn, {
  # User-experience stuff
  disable("resetParamsBtn")
  on.exit({
    enable("resetParamsBtn")
  })
  hide("errorDiv")
  
  tryCatch({
    dataValues$plate <- set_default_params(dataValues$plate)
    show("updateAdvancedSettingsDone")
    hide(id = "updateAdvancedSettingsDone", anim = TRUE,
         animType = "fade", time = 0.5, delay = 4)
  }, error = errorFunc)
})