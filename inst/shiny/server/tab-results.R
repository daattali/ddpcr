# --- Plot tab --- #

# plot button is clicked
observeEvent(input$plotBtn, {
  output$plot <- renderPlot({
    plot_params <- 
      sapply(formals(ddpcr:::plot.ddpcr_plate) %>% names,
             function(x) input[[sprintf("plot_param_%s", x)]] ) %>%
      .[!lapply(., is.null) %>% unlist]
    plot_params[['x']] <- dataValues$plate
    do.call(plot, plot_params)
  }, height = "auto")
})