library(shiny)
library(shinythemes)

navbarPage("Interest Rates",
           theme = shinytheme('yeti'),
  tabPanel("Interest Rates 1",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "rate",
          "Rates:",
          min = 0.01,
          max = 0.10,
          value = 0.05)
        ),
    mainPanel()
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
