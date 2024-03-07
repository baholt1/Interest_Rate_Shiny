library(tidyverse)
library(tidyquant)
library(purrr)
library(RTL)
library(shiny)
library(shinythemes)
library(shinipsum)
library(plotly)
library(Rcpp)
library(gridExtra)

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

## regardless of complexity and length, we can add any number of additional columns here in R
## as long as they do not call functions (i.e. dplyr::lag)
## ensures fastest execution time
calculatedData <- mycppFunction(x = as.matrix(rateData %>% mutate(date = as.numeric(date))), 0.05) %>%
  ## conversion back to data frame and grouped
  dplyr::as_tibble(res) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::group_by(maturity)

## loading a function takes the grand majority of the loading time, meaning ideal app performance is achieved with all calculations done in a single C++ file.