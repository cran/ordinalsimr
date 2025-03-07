## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

start_options <- options()

## ----setup--------------------------------------------------------------------
library(ordinalsimr)

## -----------------------------------------------------------------------------
# the default value for R installations should be 7
getOption("digits")

# this number will be printed to 7 *significant* digits
0.093573827689

## -----------------------------------------------------------------------------
# change the number of significant digits to 3
options(digits = 3)

# this number will be printed to 3 *significant* digits
0.093573827689

## ----eval=FALSE---------------------------------------------------------------
# # get all package options
# get_ordinalsimr_options()
# 
# # set the default number of iterations to 500
# opt <- options()
# set_ordinalsimr_options(
#   default_iterations = 1000,
#   default_size_min = 10,
#   default_size_max = 175,
#   default_ratio = "66:34",
#   )
# 
# # get the current value of the default number of iterations
# get_ordinalsimr_options()
# 
# # reset options
# options(opt)
# 
# # display that the options have been reset
# get_ordinalsimr_options()

## ----include = FALSE----------------------------------------------------------
options(start_options)

