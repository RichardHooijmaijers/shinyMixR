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
  expect_equal(partable$run1[1], numfmt(proj$run1$results$OBJF))
  
  # test extra options
  partable <- par_table(proj, models = "run1", projloc = paste0(tempdir(), "/files"),
                        backt = TRUE, bsv = TRUE)
  
  refchk   <-  paste0(numfmt(proj$run1$results$partbl$`Back-transformed`[1])," (",
                      numfmt(proj$run1$results$partbl$`CI Lower`[1]),", ",
                      numfmt(proj$run1$results$partbl$`CI Upper`[1]),") {",
                      numfmt(proj$run1$results$partbl$`BSV(CV%)`[1]),"}")
  expect_equal(partable$run1[3], refchk)
  
  # test output file
  partable <- par_table(proj, models = "run1", projloc = paste0(tempdir(), "/files"),
                        outnm = "par_table.html")
  
  expect_true(file.exists(paste0(tempdir(), "/files/analysis/run1/par_table.html")))
  
  #unlink(paste0(tempdir(), "/files"), recursive = TRUE) 
  rml  <- list.files(tempdir(), recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
  
})