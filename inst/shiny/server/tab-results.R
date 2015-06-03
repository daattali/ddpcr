# --- Plot tab --- #

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


output$metaTable <- DT::renderDataTable(
  dataValues$plate %>% plate_meta,
  rownames = FALSE,
  options = list(searching = FALSE)
)

output$saveMetaBtn <- downloadHandler(
  filename = function() { 
    sprintf("%s-summary.csv", dataValues$plate %>% name)
  },
  content = function(file) {
    write.csv(dataValues$plate %>% plate_meta, file, row.names = FALSE)
  }
)
