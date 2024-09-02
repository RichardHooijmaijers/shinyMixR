test_that("run_nmx works as expected", {
  
  # create project
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"),
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # get project object
  proj <- get_proj(projloc = paste0(tempdir(), "/files"))
  
  # run model
  run_nmx("run1", proj, projloc = paste0(tempdir(), "/files"))
  
  # wait for 30 seconds so model can finish
  # Double check if we want to do this here; decide if we want test-shinymixr-01-model-run-1 AND test-module_run? seems duplicated
  # This location might be the best one, the app calls this function, so if it is tested here once it should be OK!
  Sys.sleep(30)
  
  # check if output files are generated in shinyMixR directory
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.res.rds")))
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds")))
  
  # remove project
  unlink(paste0(tempdir(), "/files"), recursive = TRUE)
  
  # create new project
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"),
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # get project object
  proj <- get_proj(projloc = paste0(tempdir(), "/files"))
  
  # run model externally
  run_nmx("run1", proj, projloc = paste0(tempdir(), "/files"), ext = FALSE)
  
  # check if output files are generated in shinyMixR directory
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.res.rds")))
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds")))
  
  # remove project
  unlink(paste0(tempdir(), "/files"), recursive = TRUE)
  
})