tabPanel(
  title = "About",
  id    = "aboutTab",
  value = "aboutTab",
  name  = "aboutTab",
  class = "fade",
  icon  = icon("info-circle"),
  includeMarkdown(file.path("text", "about.md")),
  h2("Version"),
  "ddpcr R package version", as.character(utils::packageVersion("ddpcr"))
)