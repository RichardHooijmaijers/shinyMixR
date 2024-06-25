test_that("gof_plot works as expected", {
  
  res_path <- system.file("/other/run1.res.rds", package = "shinyMixR")
  
  res <- readRDS(res_path)
  
  # Note: many warnings that pollute the output
  # Note 2: the plot objects are slightly different across OS
  #         double check; is this the case or only within shinytest2 (more related to mdoel run!)
  plot <- suppressWarnings(gof_plot(res, ptype = "all", type = "xpose"))
  expect_true(is.ggplot(plot))
  expect_equal(length(plot$layers), 8)
  
  plot    <- suppressWarnings(gof_plot(res, ptype = "all", type = "user"))
  expect_true(is.ggplot(plot))
  expect_equal(length(plot$layers), 8)
  
  plotlin  <- suppressWarnings(gof_plot(res, ptype = "ipred.dv", type = "user", linscale = TRUE))
  plotlog  <- suppressWarnings(gof_plot(res, ptype = "ipred.dv", type = "user"))
  expect_true(length(plotlin$scales$scales) == 0)
  expect_true(plotlog$scales$scales[[1]]$trans$name == "log-10")
  expect_true(plotlog$scales$scales[[2]]$trans$name == "log-10")
  
  for (plot_type in c("ipred.dv", "pred.dv", "idv.res", "pred.res")) {
    
    plot <- suppressWarnings(gof_plot(res, ptype = plot_type, type = "user"))
    expect_true(is.data.frame(plot$data))
    expect_equal(nrow(plot$data), nrow(res))
    
    if (grepl("res", plot_type)) {
      expect_equal(length(plot$layers), 2)
    } else {
      expect_equal(length(plot$layers), 3)
    }
    
    if(plot_type %in% c("ipred.dv", "pred.dv")){
      expect_equal(tolower(rlang::as_name(plot$mapping$x)), sub(".*\\.","",plot_type))  
      expect_equal(tolower(rlang::as_name(plot$mapping$y)), sub("\\..*","",plot_type))  
    }
  }
  
  # test ptype = "all"
  all_plot <- suppressWarnings(gof_plot(res, ptype = "all", type = "user"))
  
  expect_true(is.ggplot(all_plot))
  expect_equal(length(all_plot$layers), 8)
  
  # test if output file is generated
  temp_dir <- tempdir()
  
  suppressWarnings(gof_plot(res, 
                            ptype = "all", 
                            type = "user", 
                            projloc = temp_dir, 
                            outnm = "gof_plot.html",
                            mdlnm = "test_model",
                            title = "myplot",
                            colby = "ID"))
  
  expect_true(file.exists(paste0(temp_dir, "/analysis/test_model/gof_plot.html")))
  
  # remove directory
  unlink(paste0(temp_dir, "/analysis"), recursive = TRUE)

})
