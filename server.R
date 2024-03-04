server <- function(input, output) {

  ## these values are user entries in the app
  coupon <- reactive({input$coupon})
  T2M <- reactive({input$T2M})
  step <- 0.0001  # in bps

  bondPrices <- reactive({
    dplyr::tibble(yield = round(seq(0.025, 0.075, step), 4)) %>%
    dplyr::mutate(price = mapply(RTL::bond,
                                 ytm = yield,
                                 C = input$coupon,
                                 T2M = input$T2M),
                  pricePlus = mapply(RTL::bond,
                                     ytm = yield + step,
                                     C = input$coupon,
                                     T2M = input$T2M),
                  priceMinus = mapply(RTL::bond,
                                      ytm = yield - step,
                                      C = input$coupon,
                                      T2M = input$T2M))
  })

  priceBond <- reactive({
    function(coupon, maturity) {
    dplyr::tibble(yield = round(seq(0.025, 0.075, step), 4)) %>%
    dplyr::mutate(price = mapply(RTL::bond,
                                  ytm = yield,
                                  C = input$coupon,
                                  T2M = input$T2M),
                    pricePlus = mapply(RTL::bond,
                                  ytm = yield + step,
                                  C = input$coupon,
                                  T2M = input$T2M),
                    priceMinus = mapply(RTL::bond,
                                  ytm = yield - step,
                                  C = input$coupon,
                                  T2M = input$T2M))
    }
  })

## the above function and the mapping below (unsliced and with all rate maturities) is what we have to do with C++
  testData <- reactive({
    rateData %>% 
    dplyr::slice(1:5) %>% 
    dplyr::mutate(bondPrice = purrr::map2(rate, maturity, priceBond())) %>% 
    tidyr::unnest(cols = bondPrice)
  })
 
  
  output$plots <- renderPlot({
    # Check which options are selected in the checkbox group
    plot_type <- input$greeks
    
    if (length(plot_type) == 0) {
      return(NULL)
    }
    
    # Generate plots based on the selected options
    plot_list <- lapply(plot_type, function(type) {
      switch(
        type,
        duration = random_ggplot("bar"),     # Replace with the appropriate pre-made chart function
        convexity = random_ggplot("histogram"),   # Replace with the appropriate pre-made chart function
        oth1 = random_ggplot("dotplot"),             # Replace with the appropriate pre-made chart function
        oth2 = random_ggplot("boxplot")              # Replace with the appropriate pre-made chart function
      )
    })
    
    # Combine multiple plots into a single plot
    grid.arrange(grobs = plot_list, ncol = 2)  # Adjust the number of columns as needed

  })
  }
