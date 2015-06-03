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


output$metaTable <- DT::renderDataTable({
  meta <- dataValues$plate %>% plate_meta(only_used = TRUE)
  colnames <- meta %>% humanFriendlyColname
  hiddenCols <- meta %>% metaColsHideIdx
  
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
                             targets = hiddenCols)),
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

metaColsHideIdx <- function(x) {
  colsHide <- c("row", "col", "used", "comment",
                "mutant_borders", "wildtype_borders", "filled_borders")
  if (all(is.na(x[['sample']]))) {
    colsHide <- c(colsHide, "sample")
  }
  which(colnames(x) %in% colsHide) - 1
}

humanFriendlyColname <- function(x) {
  x <- colnames(x)
  paste0(toupper(substring(x, 1, 1)),
         substring(gsub("_", " ", x), 2))
}