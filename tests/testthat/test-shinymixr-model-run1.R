library(shinytest2)

test_that("Shiny app runs model for run1", {
  
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Set up necessary files (internal function)
  shinyMixR:::setup_shinymixr_test(dir = "./files", 
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # Shiny test
  # Start driver
  app <- AppDriver$new(app_dir = "./files/shinyMixR/app/",
                       name = "run1-model", 
                       seed = 123)
  
  app$set_inputs(tabs = "run")
  app$set_inputs(`modrun-runLst` = "run1")
  app$click("modrun-runMdl")
  # Remove sweet alert
  app$click(selector = ".swal2-confirm")
  
  # Get initial parameter table
  initial_model_counter <- app$wait_for_value(export = "modrun-model_updated",
                                              ignore = list(NULL))
  
  app$set_inputs(tabs = "par")
  app$set_inputs(`partable-EstLst` = "run1")
  app$click(selector = "#partable-EstLst option[value='run1']")
  
  # Wait for parameter table to update
  app$wait_for_value(export = "modrun-model_updated",
                     ignore = list(initial_model_counter),
                     timeout = 120000)
  
  # Make sure the table has enough time to update
  Sys.sleep(1)
  
  app$expect_values(export = "partable-params")
  
  app$stop()
  
  # Clean up
  unlink("./files", recursive = TRUE)
  
})
