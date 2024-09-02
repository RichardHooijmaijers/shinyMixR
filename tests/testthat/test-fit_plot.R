test_that("fit_plot works as expected", {
  
  res_path <- system.file("/other/run1.res.rds", package = "shinyMixR")
  
  res <- readRDS(res_path)
  
  # Note: many warnings!
  plot <- suppressWarnings(fit_plot(res))
  
  expect_true(is.ggplot(plot))
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot), 12)
  
  # Note: many warnings!
  plot <- suppressWarnings(fit_plot(res, type = "user"))
  
  expect_true(is.ggplot(plot))
  expect_equal(length(plot$layers), 3)
  expect_equal(length(plot), 11)
  
  # Note: many warnings!
  plot <- suppressWarnings(fit_plot(res, type = "user", by = "dosenum"))
  
  expect_true(is.ggplot(plot))
  expect_equal(length(plot$layers), 3)
  expect_equal(length(plot), 11)
  
  # test if output file is generated
  temp_dir <- tempdir()
  
  suppressWarnings(fit_plot(res, 
                            type = "user", 
                            projloc = temp_dir, 
                            outnm = "fit_plot.html",
                            mdlnm = "test_model"))
  
  expect_true(file.exists(paste0(temp_dir, "/analysis/test_model/fit_plot.html")))
  
  # remove directory
  unlink(paste0(temp_dir, "/analysis"), recursive = TRUE)
  
})