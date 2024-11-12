
test_that("Shiny app creates plot and runs report", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  
  models <- paste0(temp_dir, "/files", "/models/run1.r")
  results <- paste0(temp_dir, "/files", "/shinyMixR/run1.ressum.rds")
  
  # if shinyMixR/temp directory does not exist yet, create it
  if (!dir.exists(paste0(temp_dir, "/files/shinyMixR/temp"))) {
    dir.create(paste0(temp_dir, "/files/shinyMixR/temp"), recursive = TRUE)
  }
  
  settings <- list(
    plott = "user",
    themeedt = "solarized_light",
    fontedt = 14
  )
  
  r <- list(
    active_tab = "gof",
    this_wd = paste0(temp_dir, "/files")
  )
  
  # To test report creation, first an analysis has to be made.
  # So at the same time, we are testing module_gof_server and module_fitplots_server
  # This will result in output in the analysis folder
  testServer(module_gof_server,
             args = list(settings = reactive(settings), r = r), {
               session$setInputs(gofLst = "run1",
                                 subset = "ID!=10",
                                 precode = "dataIn$DV <- log(dataIn$DV)",
                                 colby = "",
                                 ptype = "all",
                                 linscale = FALSE,
                                 showres = FALSE,
                                 savename = "GOF",
                                 typeout = "HTML")
               
               suppressWarnings({
                 # Make a new plot
                 session$setInputs(make = 1)
                 
                 # Time for the plot to render
                 Sys.sleep(2)
                 
                 # Check if plot object is present 
                 expect_equal(is.ggplot(gofplm()), TRUE)
                 
                 # Open modal
                 session$setInputs(save = 1)
                 
                 # Wait for modal to open
                 Sys.sleep(1)
                 
                 # Save results
                 session$setInputs(save2 = 1)
                 Sys.sleep(2)
                 
                 # Check if results are saved
                 expect_equal(file.exists(paste0(temp_dir, "/files/analysis/run1/GOF.html")), TRUE)
               })
             })
  
  testServer(module_fitplots_server,
             args = list(settings = reactive(settings), r = r), {
               session$setInputs(fitLst = "run1",
                                 subset = "ID!=10",
                                 precode = "dataIn$DV <- log(dataIn$DV)",
                                 by = "ID",
                                 idv = "TIME",
                                 obs = "DV",
                                 pred = "PRED",
                                 ipred = "IPRED",
                                 grp  = "ID",
                                 scales = "free",
                                 logy = FALSE,
                                 showres = FALSE,
                                 savename = "FITS",
                                 typeout = "HTML")
               
               suppressWarnings({
                 # Make a new plot
                 session$setInputs(make = 1)
                 
                 # Time for the plot to render
                 Sys.sleep(2)
                 
                 # Check if plot object is present 
                 expect_equal(is.ggplot(fitplm()), TRUE)
                 
                 # Open modal
                 session$setInputs(save = 1)
                 
                 # Wait for modal to open
                 Sys.sleep(1)
                 
                 # Save results
                 session$setInputs(save2 = 1)
                 Sys.sleep(2)
                 
                 # Check if results are saved
                 expect_equal(file.exists(paste0(temp_dir, "/files/analysis/run1/FITS.html")), TRUE)
               })
             })
  
  testServer(module_reports_server, 
             args = list(r = r),{
    
    session$setInputs(models = "run1",
                      type = "HTML",
                      showres = FALSE,
                      name = "TestReport",
                      results = c("GOF.html","FITS.html"))
    
    # Show results
    session$setInputs(showres = 1)
    
    # Test if report is created (combined when more than 1 output is available)
    expect_equal(file.exists(paste0(temp_dir, "/files/analysis/run1/TestReport.html")), TRUE)
  })
  
  #unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
  rml  <- list.files(temp_dir, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
  
})