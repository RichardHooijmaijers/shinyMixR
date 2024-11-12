test_that("run_nmx works as expected", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # create project
  unlink(paste0(tempdir(),"/files"), recursive = TRUE)
  create_proj(paste0(tempdir(),"/files"), overwrite = TRUE)
  
  # get project object
  proj <- get_proj(projloc = paste0(tempdir(), "/files"))
  
  # run model
  run_nmx("run1", proj, projloc = paste0(tempdir(), "/files"))
  
  # Wait 2 seconds to see if progress file is created
  Sys.sleep(2)
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/temp/run1.prog.txt")))
  
  # Now check if results are created
  chkt <- 1
  while (chkt<100) {
    if(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.res.rds")) &&
       file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds"))) break
    Sys.sleep(1)
    chkt <- chkt + 1
  }
  
  # check if output files are generated in shinyMixR directory
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.res.rds")))
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds")))
  
  # remove results
  unlink(paste0(tempdir(), "/files/shinyMixR/run1.res.rds"))
  unlink(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds"))
  
  # run model internally
  run_nmx("run1", proj, projloc = paste0(tempdir(), "/files"), ext = FALSE)
  
  # check if output files are generated in shinyMixR directory
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.res.rds")))
  expect_true(file.exists(paste0(tempdir(), "/files/shinyMixR/run1.ressum.rds")))
  
  # remove project
  #unlink(paste0(tempdir(), "/files"), recursive = TRUE)
  rml  <- list.files(tempdir(), recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
})
