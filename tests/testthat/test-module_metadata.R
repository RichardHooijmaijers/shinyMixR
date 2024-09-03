test_that("Shiny app runs model and returns parameters for run1", {
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  r <- list(
    this_wd = paste0(temp_dir, "/files"),
    proj_obj = get_proj(paste0(temp_dir, "/files"))
  )
  
  testServer(module_metadata_server, 
             args = list(type = "overview", selline = reactive(1), sellmod=NULL, sellcont=NULL, r = r), {
               
               # Open modal
               session$setInputs(go = 1)
               
               # Wait a bit
               Sys.sleep(2)

               # Adapt meta data ad check if results are expected
               session$setInputs(mdlimp = 3, mdldesc = "for testing", mdlref = "run1", mdldata = "theo_sd",
                                 mdlest = "focei", mdladpt = "run1", adpt = 1)
               Sys.sleep(1)
               expect_true(file.exists(paste0(r$this_wd,"/models/run1.r")))
               adpt_mdl <- readLines(paste0(r$this_wd,"/models/run1.r"))
               expect_true(any(grepl("desc.*for testing",adpt_mdl)))
               expect_true(any(grepl("imp.*3",adpt_mdl)))
               expect_true(any(grepl("est.*focei",adpt_mdl)))

             })
  
  unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
}) 
