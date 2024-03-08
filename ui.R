navbarPage(
  "Interest Rates",
  theme = shinytheme('yeti'),
  tabPanel(
    "Historic Information",
    mainPanel(
      helpText("Note: All graphs are interactive. To zoom in on a time frame, drag the box around the period.",
               "All face prices are assumed to be 100",
               "All calculations are based on period two"),
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      plotlyOutput("plot3"),
      plotlyOutput("plot4")
    )
  ),
  tabPanel(
    "Information",
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
      )
    )
  ),
  tabPanel(
    "Portfolio",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "maturity",
          "Select the Maturity:",
          choices = unique(calculatedData$maturity),
          selected = unique(calculatedData$maturity)
        )
      ),
      dataTableOutput("dtframe"
      )
    )
  )
)