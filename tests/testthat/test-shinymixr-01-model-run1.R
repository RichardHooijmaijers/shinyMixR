library(shinytest2)

test_that("Shiny app runs model and returns parameters for run1", {
  
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Set up necessary files (internal function)
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"),
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # Start driver for Shiny test
  app <- AppDriver$new(app_dir = paste0(tempdir(),"/files/shinyMixR/app/"), 
                       name = "run1-model", 
                       seed = 123)
  
  app$set_inputs(tabs = "run")
  app$set_inputs(`modrun-runLst` = "run1")
  app$click("modrun-runMdl")
  # Add small timeout so sweet alert has time to display and click confirm
  Sys.sleep(1)
  app$click(selector = ".swal2-confirm")
  
  # Wait for model to finish (NULL or 0 is the initial value, so we ignore it)
  app$wait_for_value(export = "modrun-model_updated",
                     ignore = list(NULL, 0),
                     timeout = 240000)
  
  # Test if run is done and 'correct' results have been created
  Sys.sleep(1)
  rundone <- "run1.res.rds"%in%list.files(paste0(tempdir(),"/files/shinyMixR/app/shinyMixR"))
  expect_true(rundone)
  if(rundone){
    runres <- readRDS(paste0(tempdir(),"/files/shinyMixR/app/shinyMixR/run1.res.rds"))
    expect_true(inherits(runres,"nlmixr2FitData"))
  }
  
  # Stop and clean up
  app$stop()
  unlink(paste0(tempdir(),"/files"), recursive = TRUE) 
})
