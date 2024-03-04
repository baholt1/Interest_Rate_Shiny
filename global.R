library(tidyverse)
library(tidyquant)
library(purrr)
library(RTL)
library(shiny)
library(shinythemes)
library(shinipsum)
library(plotly)

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
  dplyr::filter(maturity == 1)## trimmed down from full vector of maturities. Remove once c++ function is done
  
## final dataframe that results from the FRED dataframe being fed into the bondsuite_calculation C++ function
# calculatedData <- bondsuite_calculation(x = as.matrix(rateData %>% dplyr::mutate(date = as.numeric(date)))) %>% # Dataframe must be fed into the function as a matrix to comply with C++, does not support Date type meaning that column must be converted to numeric
#   ## converting back into a dataframe, reverting the Date column back to its proper type, grouping by maturity
#   dplyr::as_tibble(res) %>%
#   dplyr::mutate(date = as.Date(date)) %>%
#   dplyr::group_by(maturity)

## loading a function takes the grand majority of the loading time, meaning ideal app performance is achieved with all calculations done in a single C++ file.