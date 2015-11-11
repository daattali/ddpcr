tabPanel(
  title = "About",
  id    = "aboutTab",
  value = "aboutTab",
  name  = "aboutTab",
  icon  = icon("info-circle"),
  includeMarkdown(file.path("text", "about.md"))
)