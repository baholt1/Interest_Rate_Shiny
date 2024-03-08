server <- function(input, output) {
  
  calculatedData <- reactive({mycppFunction(x = as.matrix(rateData %>% mutate(date = as.numeric(date))), 0.05) %>%
    ## conversion back to data frame and grouped
    dplyr::as_tibble(res) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(maturity)
  })
 
  output$table <- shiny::renderDataTable({
    calculatedData()
  }) 

  
  rateFig <- reactive({
    fig1 <- calculatedData() %>% 
      dplyr::mutate(maturity = as.character(maturity)) %>% 
      ggplot(aes(x = date, y = rate, col = maturity)) + 
      geom_line() +
      theme_minimal() + 
      theme(axis.line = element_line(color = "black"), 
            axis.ticks = element_line(color = "black"), 
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            axis.title.x = element_text(color = "black", size = 10), 
            axis.title.y = element_text(color = "black", size = 10)) + 
      scale_color_discrete(name = "Maturities") + 
      labs(x = "Time", y = "Interest Rate")
    fig1 <- ggplotly(fig1)
    fig1
  })
  
  changeFig <- reactive({
    fig2 <- calculatedData() %>% 
      dplyr::mutate(maturity = as.character(maturity)) %>% 
      ggplot(aes(x = date, y = changeBPS, col = maturity)) +
      geom_point() +
      theme_minimal() + 
      theme(axis.line = element_line(color = "black"), 
            axis.ticks = element_line(color = "black"), 
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            axis.title.x = element_text(color = "black", size = 10), 
            axis.title.y = element_text(color = "black", size = 10)) + 
      scale_color_discrete(name = "Maturities") + 
      labs(x = "Time", y = "Price Change in Basis Points")
    fig2 <- ggplotly(fig2)
    fig2
  })
  
  deltaFig <- reactive({
    fig3 <- calculatedData() %>% 
      dplyr::mutate(maturity = as.character(maturity)) %>% 
      ggplot(aes(x = date, y = delta, col = maturity)) + 
      geom_line() +
      theme_minimal() + 
      theme(axis.line = element_line(color = "black"), 
            axis.ticks = element_line(color = "black"), 
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            axis.title.x = element_text(color = "black", size = 10), 
            axis.title.y = element_text(color = "black", size = 10)) + 
      scale_color_discrete(name = "Maturities") + 
      labs(x = "Time", y = "Delta")
    fig3 <- ggplotly(fig3)
    fig3
  })
  
  gammaFig <- reactive({
    fig4 <- calculatedData() %>% 
      dplyr::mutate(maturity = as.character(maturity)) %>% 
      ggplot(aes(x = date, y = gamma, col = maturity)) + 
      geom_line() +
      theme_minimal() + 
      theme(axis.line = element_line(color = "black"), 
            axis.ticks = element_line(color = "black"), 
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            axis.title.x = element_text(color = "black", size = 10), 
            axis.title.y = element_text(color = "black", size = 10)) + 
      scale_color_discrete(name = "Maturities") + 
      labs(x = "Time", y = "Gamma")
    fig4 <- ggplotly(fig4)
    fig4
  })
  
  output$plot1 <- renderPlotly({rateFig()})
  output$plot2 <- renderPlotly({changeFig()})
  output$plot3 <- renderPlotly({deltaFig()})
  output$plot4 <- renderPlotly({gammaFig()})
  
  
  output$dtframe <- DT::renderDataTable({
    filtered_data <- calculatedData[calculatedData$maturity %in% input$maturity, ]
    DT::datatable(filtered_data)
  })
  

}