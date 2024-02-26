library(tidyverse)
library(tidyquant)
library(RTL)
library(purrr)

### Some of the code below will be moved to server.R
## use full vector when c++ function is done
tickers <- c("DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

## collecting FRED data and transforming to %rates and change in bps
rateData <- tidyquant::tq_get(tickers, 
                              get = "economic.data",
                              from = "1992-01-01",
                              to = Sys.Date()) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(date,
                maturity = as.numeric(stringr::str_replace_all(symbol, "(?i)DGS", "")),
                rate = price / 100, .keep = "none") %>% 
  dplyr::group_by(maturity) %>% 
  dplyr::mutate(changeBPS = (rate - dplyr::lag(rate)) * 10000) %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(maturity == 1) ## trimmed down from full vector of maturities. Remove once c++ function is done


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
