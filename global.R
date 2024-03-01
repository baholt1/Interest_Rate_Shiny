library(tidyverse)
library(tidyquant)
library(purrr)
library(RTL)
library(shiny)
library(shinythemes)

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



