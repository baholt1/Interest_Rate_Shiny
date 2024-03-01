server <- function(input, output) {

  ## these values can be user entries in the app
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
    dplyr::mutate(bondPrice = purrr::map2(rate, maturity, priceBond)) %>% 
    tidyr::unnest()
  })
 
  output$table <- shiny::renderDataTable({
    testData()
  }) 
  
}