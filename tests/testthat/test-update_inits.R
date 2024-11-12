test_that("update_inits works as expected", {
  
  # set temp dir
  temp_dir <- tempdir()
  
  script <- system.file("other", "run1.r", package = "shinyMixR")
  res <- system.file("other", "run1.res.rds", package = "shinyMixR")
  
  update_inits(readLines(script), res, paste0(temp_dir, "/run2.r"))
  
  # check if file has been written
  expect_true(file.exists(paste0(temp_dir, "/run2.r")))
  
  # unlink file
  unlink(paste0(temp_dir, "/run2.r"))
  
})