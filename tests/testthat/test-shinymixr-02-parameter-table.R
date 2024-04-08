library(shinytest2)

test_that("Shiny app creates correct parameter table", {
  
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, incres = TRUE)
  
  # Start driver for Shiny test
  app <- AppDriver$new(app_dir = paste0(tempdir(),"/files/shinyMixR/app/"), 
                       name = "run2-model", 
                       seed = 123)
  
  app$set_inputs(tabs = "par")
  app$set_inputs(`partable-EstLst` = "run1")
  app$click(selector = "#partable-EstLst option[value='run1']")
  app$expect_values(export = "partable-params")
  
  app$expect_values(export = "partable-params")
  
  # Stop and clean up
  app$stop()
  unlink(paste0(tempdir(),"/files"), recursive = TRUE) 
})