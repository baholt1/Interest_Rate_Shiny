navbarPage(
  "Interest Rates",
  theme = shinytheme('yeti'),
  tabPanel(
    "Historic Information",
    mainPanel(
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      plotlyOutput("plot3"),
      plotlyOutput("plot4")
    )
  ),
  tabPanel(
    "Risk Appetite",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "userrate",
          "Choose a Duration:",
          choices = tickers 
        )
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