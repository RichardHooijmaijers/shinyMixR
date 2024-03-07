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
  Sys.sleep(60)
  app$set_inputs(tabs = "par")
  app$set_inputs(`partable-EstLst` = "run1")
  
  app$expect_values(output = "partable-EstTbl")
  
  app$stop()
  
  # Clean up
  unlink("./files", recursive = TRUE)
  
})
