test_that("Shiny app runs model", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function)
  unlink(paste0(temp_dir,"/files"), recursive = TRUE)
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = FALSE)
  
  # if shinyMixR/temp directory does not exist yet, create it
  if (!dir.exists(paste0(temp_dir, "/files/shinyMixR/temp"))) {
    dir.create(paste0(temp_dir, "/files/shinyMixR/temp"), recursive = TRUE)
  }
  
  r <- list(
    active_tab = "run",
    this_wd = paste0(temp_dir, "/files"),
    proj_obj = get_proj(paste0(temp_dir, "/files"))
  )
  
  testServer(module_run_server, 
             args = list(r = r), {
               
               # Set inputs
               session$setInputs(runLst = "run1")
               
               # Run model
               session$setInputs(runMdl = 1)
               
               # Wait a bit
               Sys.sleep(2)
               
               # Check if progress file is present
               expect_true(file.exists(paste0(temp_dir, "/files/shinyMixR/temp/run1.prog.txt")))
               
               # Open monitoring modal
               session$setInputs(monMdl = 1)
               
               # Test if a model can be correctly submitted also within shiny
               chkt <- 1
               while (chkt<100) {
                 if(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds"))) break
                 Sys.sleep(1)
                 chkt <- chkt + 1
               }
               expect_true(file.exists(paste0(temp_dir, "/files/shinyMixR/run1.ressum.rds")))
             })
  
  #unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
  rml  <- list.files(temp_dir, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
  
})
