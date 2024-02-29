## these values can be user entries in the app
coupon <- 0.05
T2M <- 5
step <- 0.0001  # in bps

bondPrices <- dplyr::tibble(yield = round(seq(0.025, 0.075, step), 4)) %>%
  dplyr::mutate(price = mapply(RTL::bond,
                               ytm = yield,
                               C = coupon,
                               T2M = T2M),
                pricePlus = mapply(RTL::bond,
                                   ytm = yield + step,
                                   C = coupon,
                                   T2M = T2M),
                priceMinus = mapply(RTL::bond,
                                    ytm = yield - step,
                                    C = coupon,
                                    T2M = T2M))

priceBond <- function(coupon, maturity) {
  dplyr::tibble(yield = round(seq(0.025, 0.075, step), 4)) %>%
    dplyr::mutate(price = mapply(RTL::bond,
                                 ytm = yield,
                                 C = coupon,
                                 T2M = T2M),
                  pricePlus = mapply(RTL::bond,
                                     ytm = yield + step,
                                     C = coupon,
                                     T2M = T2M),
                  priceMinus = mapply(RTL::bond,
                                      ytm = yield - step,
                                      C = coupon,
                                      T2M = T2M))
}

## the above function and the mapping below (unsliced and with all rate maturities) is what we have to do with C++
testData <- rateData %>% 
  dplyr::slice(1:5) %>% 
  dplyr::mutate(bondPrice = purrr::map2(rate, maturity, priceBond)) %>% 
  tidyr::unnest()
