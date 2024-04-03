# see https://github.com/rstudio/shinytest2/issues/351 
options(chromote.timeout = 120)

library(testthat)
library(shinyMixR)

test_check("shinyMixR")
