library(shinytest2)

test_that("Shiny app correctly creates new model code", {
  
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  
  # Start driver for Shiny test
  shinyMixR::run_shinymixr(paste0(tempdir(),"/files"), write_to_file = TRUE)
  
  app <- AppDriver$new(app_dir = paste0(tempdir(),"/files"), 
                       name = "run4-script", 
                       seed = 123)
  
  # open run script modal
  app$click("oview-runscripts-runscript")
  
  # choose file
  app$set_inputs("oview-runscripts-files" = "run1.r")
  
  # run script
  app$click("oview-runscripts-runscriptA")
  
  # Capture state for debugging
  app$expect_values(input = "editor-newmdl")
  
  # Wait for modal to open
  Sys.sleep(1)
  app$click("editor-newgo")
  
  # Capture state for debugging
  app$expect_values(input = "editor-newgo")

  # Stop and clean up
  app$stop()
  unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
})