navbarPage(
  "Interest Rates",
  theme = shinytheme('yeti'),
  tabPanel(
    "Historic Information",
    mainPanel(
      helpText("Note: All graphs are interactive. To zoom in on a time frame, drag the box around the period.",
               "All face prices are assumed to be 100.",
               "All calculations are based on period two."),
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      plotlyOutput("plot3"),
      helpText("Delta represents the sensitivity of the bond to changes in the interest rate."),
      plotlyOutput("plot4"),
      helpText("Gamma measures how the duration of a bond changes as a result of interest rates."),
      plotlyOutput("plot5"),
      helpText("Convexity measures the curvature of the relationship between bond prices and changes in interest rates.")
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
      tableOutput("dtframe"
      )
    )
  )
)