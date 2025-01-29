## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ordinalsimr)

## -----------------------------------------------------------------------------
output <- readRDS("data-2025-01-19-d8621b-1.rds")
output$comparison_data$distribution_statistics
str(output, max.level = 2)

