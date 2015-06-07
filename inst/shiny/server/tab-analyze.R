# ddPCR R package - Dean Attali 2015
# --- Analyze tab server --- #

# analyze button is clicked
observeEvent(input$analyzeBtn, {
  withBusyIndicator("analyzeBtn", {
    text("analyzeProgress", "")
    withCallingHandlers(
      dataValues$plate <- dataValues$plate %>% analyze(restart = TRUE),
      message = function(m) {
        text("analyzeProgress", m$message, TRUE)
      }
    )
    show("analyzeNextMsg")
  })
})

# change to results tab when clicking on link
observeEvent(input$toResults,
  updateTabsetPanel(session, "mainNav", "resultsTab")
)