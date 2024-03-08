shiny::navbarPage(
  "Interest Rates",
  theme = shinytheme('yeti'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  shiny::tabPanel(
    "Bond Portfolio Creation Tool",
    shiny::headerPanel("Customize Your Own Bond Portfolio"),
    shiny::helpText("Select multiple different maturities and assign a weight for each to create your optimal portfolio."),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::checkboxGroupInput(
          "maturity",
          "Please select the different maturities for your portfolio (values in years):",
          choices = unique(calculatedData$maturity),
          selected = head(unique(calculatedData$maturity), 3)
        ), 
        shiny::uiOutput("weight_inputs"),
        shiny::actionButton("calculate", "Calculate")
      ),
      shiny::mainPanel(
        shiny::tableOutput("dtframe"),
        shiny::textOutput("portfolio_ytm"),
        shiny::textOutput("total_weight")
      )
    )
  ),
  shiny::tabPanel(
    "Historic Information",
    shiny::headerPanel("Historic Information For Each Maturity"),
    shiny::helpText("All graphs are interactive. To zoom in on a time frame, drag the box around the period. To select a maturity click that maturity on the right hand side of the graph. To unselect, click the maturity again."),
    shiny::mainPanel(
      shiny::helpText("Note: All face prices are assumed to be 100.",
               "All calculations are based on period two."),
      plotly::plotlyOutput("plot1"),
      plotly::plotlyOutput("plot2"),
      plotly::plotlyOutput("plot3"),
      shiny::helpText("Delta represents the sensitivity of the bond to changes in the interest rate."),
      plotly::plotlyOutput("plot4"),
      shiny::helpText("Gamma measures how the duration of a bond changes as a result of interest rates."),
      plotly::plotlyOutput("plot5"),
      shiny::helpText("Convexity measures the curvature of the relationship between bond prices and changes in interest rates.")
    )
  )
)

