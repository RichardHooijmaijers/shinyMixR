test_that("Shiny app runs model and returns parameters for run1", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  
  models <- paste0(temp_dir, "/files", "/models/run1.r")
  
  scripts <- paste0(temp_dir, "/files", 
                    c("/scripts/combined.results.html.r",
                      "/scripts/eta.plot.r",
                      "/scripts/vpc.plot.r"))
  
  # if shinyMixR/temp directory does not exist yet, create it
  if (!dir.exists(paste0(temp_dir, "/files/shinyMixR/temp"))) {
    dir.create(paste0(temp_dir, "/files/shinyMixR/temp"), recursive = TRUE)
  }
  
  r <- list(
    uids_running = 0,
    this_wd = paste0(temp_dir, "/files"),
    scrpt = scripts
  )
  
  testServer(module_scripts_server, 
             args = list(files = reactive(models), loc = paste0(temp_dir, "/files/shinyMixR/temp"), r = r), {
               
               # Open modal
               session$setInputs(runscript = 1)
               
               # Wait a bit
               Sys.sleep(2)
               
               # Choose script, file and run script
               session$setInputs(files = "run1.r",
                                 scripts = "eta.plot.r",
                                 runscriptA = 1)

               # Wait a bit
               Sys.sleep(2)
               
               expect_equal(output$scriptprogress, "Run eta.plot.r for file(s) run1.r")
               
             })
  
  #unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
  rml  <- list.files(temp_dir, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
  
}) 