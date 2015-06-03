# --- Analyze tab --- #

# analyze button is clicked
observeEvent(input$analyzeBtn, {
  # User-experience stuff
  disable("analyzeBtn")
  on.exit({
    enable("analyzeBtn")
  })
  hide("errorDiv")
  
  tryCatch({
    text("analyzeProgress", "")
    withCallingHandlers(
      dataValues$plate <- dataValues$plate %>% analyze(restart = TRUE),
      message = function(m) {
        text("analyzeProgress", m$message, TRUE)
      }
    )
    show("analyzeDone")
    hide(id = "analyzeDone", anim = TRUE,
         animType = "fade", time = 0.5, delay = 4)
  }, error = errorFunc)
})