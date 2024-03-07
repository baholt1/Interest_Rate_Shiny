navbarPage("Interest Rates",
           theme = shinytheme('yeti'),
  tabPanel("Historic Information",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "userrate",
          "Choose a Duration:",
          choices = tickers
        ),
        sliderInput(
          "coupon",
          "Coupon Rates:",
          min = 0,
          max = 0.10,
          value = 0.01),
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
  tabPanel("Risk Appetite",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "userrate",
          "Choose a Duration:",
          choices = tickers
        ),
        checkboxGroupInput(
          "greeks",
          "Choose your risk appetite:",
          c("Rates" = "rates",
            "Change in Basis Points" = "changeBPS",
            "Delta" = "delta",
            "Gamma" = "gamma"),
          selected = "rates"),
          ),
      mainPanel(
        plotOutput("plots")
      ),
      )
    ),
  tabPanel("Portfolio",
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
             mainPanel()
          )
    )
)
