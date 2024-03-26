test_that("gof_plot works", {
  
  res_path <- system.file("/Other/run1.res.rds", package = "shinyMixR")
  
  res <- readRDS(res_path)
  
  # Note: many warnings that pollute the output
  plot <- suppressWarnings(gof_plot(res, ptype = "all", type = "xpose"))
  expect_true(is.ggplot(plot))
  expect_equal(length(plot), 9)
  
  plot <- suppressWarnings(gof_plot(res, ptype = "all", type = "user"))
  expect_true(is.ggplot(plot))
  expect_equal(length(plot), 9)
  
  for (plot_type in c("ipred.dv", "pred.dv", "idv.res", "pred.res")) {
    
    plot <- suppressWarnings(gof_plot(res, ptype = plot_type, type = "xpose"))
    expect_true(is.data.frame(plot$data))
    expect_equal(nrow(plot$data), nrow(res))
    if (grepl("res", plot_type)) {
      expect_equal(length(plot$layers), 4)
    } else {
      expect_equal(length(plot$layers), 5)
    }
  }

})