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
      labs(x = "Time", 
           y = "Interest Rate", 
           title = "Change in Interest Rates Over Time")
    fig1 <- ggplotly(fig1)
    fig1
  })
  
  changeFig <- reactive({
    fig2 <- calculatedData() %>%
      dplyr::filter(changeBPS < 100) %>% 
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
      labs(x = "Time",
           y = "Price Change in Basis Points",
           title = "Change in Basis Points")
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
      labs(x = "Time",
           y = "Delta",
           title = "Delta")
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
      labs(x = "Time",
           y = "Gamma", 
           title = "Gamma")
    fig4 <- ggplotly(fig4)
    fig4
  })
  
  convexityFig <- reactive({
    fig5 <- calculatedData() %>% 
      dplyr::mutate(maturity = as.character(maturity)) %>% 
      ggplot(aes(x = date, y = convexity, col = maturity)) + 
      geom_line() +
      theme_minimal() + 
      theme(axis.line = element_line(color = "black"), 
            axis.ticks = element_line(color = "black"), 
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            axis.title.x = element_text(color = "black", size = 10), 
            axis.title.y = element_text(color = "black", size = 10)) + 
      scale_color_discrete(name = "Maturities") + 
      labs(x = "Time",
           y = "Convexity", 
           title = "Convexity")
    fig5 <- ggplotly(fig5)
    fig5
  })
  
  output$plot1 <- renderPlotly({rateFig()})
  output$plot2 <- renderPlotly({changeFig()})
  output$plot3 <- renderPlotly({deltaFig()})
  output$plot4 <- renderPlotly({gammaFig()})
  output$plot5 <- renderPlotly({convexityFig()})
  
  ytm <- function(PVs, Ms, C) {
    ytms <- numeric(length(PVs))
    for (i in 1:length(PVs)) {
      ytm_1 <- (C + (100 - PVs[i]) / Ms[i])
      ytm_2 <- (100 + PVs[i]) / 2
      ytms[i] <- ytm_1 / ytm_2
    }
    return(ytms)
  }

  ytms_reactive <- reactive({
    C <- c(5, 5, 5, 5, 5, 5, 5, 5)
    Ms <- input$maturity
    
    series <- ifelse(Ms == 1, "DGS1",
                  ifelse(Ms == 2, "DGS2",
                    ifelse(Ms == 3, "DGS3",
                      ifelse(Ms == 5, "DGS5",
                        ifelse(Ms == 7, "DGS7",
                          ifelse(Ms == 10, "DGS10",
                            ifelse(Ms == 20, "DGS20",
                              ifelse(Ms == 30, "DGS30", NA))))))))
    
    PVs <- tq_get(series, 
                  get = "economic.data", 
                  from = "2024-01-01", 
                  to = Sys.Date()) %>% 
      group_by(symbol) %>% 
      mutate(value = 100 - price) %>% 
      pivot_wider(names_from = symbol, values_from = value) %>% 
      filter(date == "2024-03-04")
    
    data_long <- pivot_longer(PVs, cols = -c(date, price), names_to = "bond", values_to = "value") %>% 
      na.omit() 
    
    PVs <- data_long$value
    
    ytms <- ytm(PVs, Ms, C)
    data.frame(Bond = data_long$bond, YTM = ytms)
  })
  
  output$dtframe <- DT::renderDataTable({
    datatable(ytms_reactive())
  })

}