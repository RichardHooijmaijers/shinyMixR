library(shinytest2)

test_that("Shiny app creates run2 and shows Goodness of Fit plots", {
  
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Set up necessary files (internal function)
  shinyMixR:::setup_shinymixr_test(dir = "./files", 
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # Shiny test
  # Start driver
  app <- AppDriver$new(app_dir = "./files/shinyMixR/app/",
                       name = "run2-model", 
                       seed = 123)
  
  app$set_inputs(tabs = "editor")
  app$click("editor-newmdl")
  app$click("editor-newgo")
  
  app$set_inputs(tabs = "run")
  app$set_inputs(`modrun-runLst` = "run2")
  app$click("modrun-runMdl")
  # Remove sweet alert
  app$click(selector = ".swal2-confirm")
  
  # Get initial value of model_updated reactive val
  initial_model_counter <- app$wait_for_value(export = "modrun-model_updated",
                                              ignore = list(NULL))
  
  # Wait for model to finish
  app$wait_for_value(export = "modrun-model_updated",
                     ignore = list(initial_model_counter),
                     timeout = 120000)
  
  app$set_inputs(tabs = "gof")
  app$set_inputs(`gofplots-gofLst` = "run2")
  app$click(selector = "#gofplots-gofLst option[value='run2']")
  app$click(selector = "#gofplots-make")
  
  # Make sure the plots has enough time to update
  app$wait_for_value(export = "gofplots-plot_updated",
                     ignore = list(NULL),
                     timeout = 5000)
  
  # Note: we can't expect values for gofplots-plot_updated: cannot be serialized to JSON
  app$expect_values(output = "gofplots-gof_plot")
  
  app$stop()
  
  # Clean up
  unlink("./files", recursive = TRUE)
  
})
