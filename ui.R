navbarPage("Interest Rates",
           theme = shinytheme('yeti'),
  tabPanel("Interest Rates 1",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "coupon",
          "Coupon Rates:",
          min = 0.01,
          max = 0.10,
          value = 0.05),
        sliderInput(
          "T2M",
          "Time to Maturity:",
          min = 1,
          max = 30,
          value = 10),
        ),
    mainPanel(
      dataTableOutput("table"))
    )
  ),
  tabPanel("Interest Rates 2",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "userrate",
          "Choose a Duration:",
          choices = tickers
        )
      ),
      mainPanel()
    )
  )
)
