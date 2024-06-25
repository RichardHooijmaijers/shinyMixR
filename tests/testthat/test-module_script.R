
test_that("Shiny app runs given R script", {

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

  testServer(module_scripts_server,
             args = list(files = reactive(models), scripts = reactive(scripts), loc = paste0(temp_dir, "/files/shinyMixR/temp")), {

               print('got here')
               # Open modal
               session$setInputs(runscript = 1)

               # Choose script and file
               session$setInputs(files = "run1.r")
               session$setInputs(scripts = "eta.plot.r")
               
               print('got here2')

               # Run script
               session$setInputs(runscriptA = 1)
               
               print('got here 3')

               # Wait a bit
               Sys.sleep(2)

               # Check if scriptres output is present
               expect_equal(monout(), TRUE)

               expect_equal(output$scriptprogress, "Run eta.plot.r for file(s) run1.r")

             })

  unlink(paste0(temp_dir, "/files"), recursive = TRUE)

})