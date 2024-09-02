# see https://github.com/rstudio/shinytest2/issues/351 
options(chromote.timeout = 120)

library(testthat)
library(shinyMixR)
library(nlmixr2)

test_check("shinyMixR")
# devtools::test() 
# test_dir("tests/testthat/")
# devtools::test_active_file(file = "tests/testthat/test-gof_plot.R")
# devtools::test_active_file(file = "tests/testthat/test-shinymixr-01-model-run1.R")
# devtools::test_active_file(file = "tests/testthat/test-shinymixr-02-parameter-table.R")
# devtools::test_active_file(file = "tests/testthat/test-shinymixr-03-create-newmodel.R")
# covr::report()
# covr::package_coverage()
# covr::codecov()
