library(tidyquant)
library(shiny)
library(shinythemes)
library(plotly)
library(Rcpp)
library(gridExtra)
library(tidyr)

# Load the C++ function
sourceCpp('testCplus.cpp')

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
                rate = price / 100, .keep = "none")


calculatedData <- mycppFunction(x = as.matrix(rateData %>% mutate(date = as.numeric(date))), 0.05) %>%
  dplyr::as_tibble(res) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::group_by(maturity)




