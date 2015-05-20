tabPanel(
  title = "Settings",
  id = "settingsTab",
  value = "settingsTab",
  name = "settingsTab",

  textInput("settingsSubset",
            span("Keep only certain wells ",
            a(href = "#",
              `data-toggle` = "popover",
              `data-title` = "Range notation",
              `data-content` = "You can select multiple wells using a special \"range notation\". Non-adjacent wells can be specified by separating them with commas. Adjacent wells can be selected with the colon operator, which will select all wells in the rectangle defined by the two end-points. For example, \"B01, C04:D06, F10\" will select wells B01, C04, C05, C06, D04, D05, D06, F10.",
              `data-trigger` = "hover",
              icon("question-circle"))),
            "A01:H12"),
  actionLink("settingsShowAllWells", "Show all available wells"), br(),
  hidden(span(id = "settingsAllWells", 
              plotOutput("wellsUsedPlot"))),
  br(), br(),
  textInput("settingsXvar", "Dye along X axis", ""),
  textInput("settingsYvar", "Dye along Y axis", ""),
  br(),
  actionButton(
    "updateSettings",
    "Update",
    class = "btn-primary"
  )
)