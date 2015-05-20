# This file contains the UI for the tab that plots the dataset

tabPanel(
  title = "Plot",
  id = "plotTab",
  value = "plotTab",
  
  sidebarLayout(
    sidebarPanel(
      h3("Plot parameters"),
      lapply(
        formals(ddpcrS3:::plot.ddpcr_plate) %>% names,
        function(param_name) {
          param_val <- formals(ddpcrS3:::plot.ddpcr_plate) %>% .[[param_name]]
          param_id <- sprintf("plot_param_%s", param_name)
          if (missing(param_val) || param_val %>% is.language) {
            return()
          } else if (param_val %>% is.logical) {
            input_type <- checkboxInput
          } else if (param_val %>% is.numeric) {
            input_type <- numericInput
          } else {
            input_type <- textInput
          }
          do.call(input_type, list(param_id, param_name, param_val))
        }
      )
    ),
    
    mainPanel(
      actionButton(
        "plotBtn",
        "Plot",
        class = "btn-primary"
      ),
      plotOutput("plot")
    )
  )
)