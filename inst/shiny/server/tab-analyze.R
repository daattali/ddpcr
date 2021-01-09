# ddPCR R package - Dean Attali 2015
# --- Analyze tab server --- #

# analyze button is clicked
observeEvent(input$analyzeBtn, {
  withBusyIndicator("analyzeBtn", {
    html("analyzeProgress", "")
    withCallingHandlers(
      dataValues$plate <- dataValues$plate %>% analyze(restart = TRUE),
      message = function(m) {
        html("analyzeProgress", m$message, TRUE)
      },
      warning = function(m) {
        # Don't show all the dplyr deprecation warnings
        if (!grepl("dplyr 0.7", m$message)) {
          html("analyzeProgress", paste0(m$message, "\n"), TRUE)
        }
      }
    )
    show("analyzeNextMsg")
  })
})

# change to results tab when clicking on link
observeEvent(input$toResults,
  updateTabsetPanel(session, "mainNav", "resultsTab")
)
