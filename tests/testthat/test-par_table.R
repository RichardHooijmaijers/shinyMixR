test_that("partable works", {
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  
  # get project object
  proj <- get_proj(projloc = paste0(tempdir(), "/files"))
  
  # test basics
  partable <- par_table(proj, models = "run1", projloc = paste0(tempdir(), "/files"))
  
  expect_equal(nrow(partable), 6)
  expect_equal(ncol(partable), 2)
  expect_equal(partable$Parameter[1], "OBJF")
  expect_equal(partable$run1[1], "1042.61")
  
  # test extra options
  partable <- par_table(proj, models = "run1", projloc = paste0(tempdir(), "/files"),
                        backt = TRUE, bsv = TRUE)
  
  expect_equal(partable$run1[3], "0.12 (0.072, 0.2) {108.}")
  
  # test output file
  partable <- par_table(proj, models = "run1", projloc = paste0(tempdir(), "/files"),
                        outnm = "par_table.html")
  
  expect_true(file.exists(paste0(tempdir(), "/files/analysis/run1/par_table.html")))
  
  unlink(paste0(tempdir(), "/files"), recursive = TRUE) 
  
})