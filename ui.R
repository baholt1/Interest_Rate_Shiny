navbarPage("Interest Rates",
           theme = shinytheme('yeti'),
  tabPanel("Historic Information",
    h2("Historic Information"),
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
          min = 0.01,
          max = 0.10,
          value = 0.05),
        sliderInput(
          "T2M",
          "Time to Maturity:",
          min = 1,
          max = 30,
          value = 10)
        ),
    mainPanel( 
      dataTableOutput("table"))
    )
  ),
  tabPanel("Risk Appetite",
    h2("Risk Appetite"),       
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
          c("Duration" = "duration",
          "Convexity" = "convexity",
          "other 1" = "oth1",
          "other 2" = "oth2"),
          selected = "duration"
      )
      ),
      mainPanel(
        plotOutput("plots"))
  )
  ),
  tabPanel("Portfolio",
           h2("Portfolio"),
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
                 value = 10)
             ),
             mainPanel()
          )
    )
)
