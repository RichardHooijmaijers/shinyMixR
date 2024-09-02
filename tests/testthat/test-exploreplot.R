test_that("exploreplot works as expected", {
  
  res_path <- system.file("/other/run1.res.rds", package = "shinyMixR")
  
  res <- readRDS(res_path)
  
  input <- list(
    mdls = "run1",
    Xval1 = "TIME",
    Yval1 = "DV",
    geoms1 = "line",
    stats1 = "[empty]",
    fcol1 = "default",
    group1 = "ID",
    colour1 = "dosenum",
    shape1 = "[empty]",
    size1 = "[empty]",
    label1 = "[empty]",
    Xval2 = "[empty]",
    Yval2 = "[empty]",
    geoms2 = "[empty]",
    stats2 = "[empty]",
    fcol2 = "default",
    group2 = "[empty]",
    colour2 = "[empty]",
    shape2 = "[empty]",
    size2 = "[empty]",
    label2 = "[empty]",
    Xval3 = "[empty]",
    Yval3 = "[empty]",
    geoms3 = "[empty]",
    stats3 = "[empty]",
    fcol3 = "default",
    group3 = "[empty]",
    colour3 = "[empty]",
    shape3 = "[empty]",
    size3 = "[empty]",
    label3 = "[empty]",
    nondups = "",
    facet1 = "[empty]",
    facet2 = "[empty]",
    facet3 = "[empty]",
    facetsc = "fixed",
    use_input = FALSE,
    stack = TRUE,
    Xfact = FALSE,
    Yfact = FALSE,
    Ylog = FALSE,
    Xlog = FALSE,
    omitSE = FALSE,
    fsize1 = 1,
    falph1 = 1,
    fsize2 = 1,
    falph2 = 1,
    fsize3 = 1,
    falph3 = 1,
    plheight = 600,
    ncol = NA,
    xlim1 = NA,
    xlim2 = NA,
    ylim1 = NA,
    ylim2 = NA,
    refint = NA,
    refslope = NA,
    vref = NA,
    subset = "",
    precode = "",
    ptitle = "run1",
    xlab = "",
    ylab = ""
  )
  
  out <- exploreplot(input)
  
  expect_true(grepl("ggplot", out))
  expect_true(grepl(paste0("geom_", input$geoms1), out))
  expect_true(grepl(input$ptitle, out))
  expect_true(grepl(paste0("group=", input$group1), out))
  
  # add stats 1
  input$stats1 <- "mean"
  out <- exploreplot(input)
  expect_true(grepl(paste0("fun=", input$stats1), out))
  
  # add stats 1
  input$stats1 <- "mean (SD)"
  out <- exploreplot(input)
  expect_true(grepl("fun=mean", out))
  expect_true(grepl("fun.min", out))
  expect_true(grepl("fun.max", out))
  
  # add stats 1
  input$stats1 <- "median (5-95th perc.)"
  out <- exploreplot(input)
  expect_true(grepl("quantile", out))
  
  # change nondups
  input$nondups <- "TIME"
  out <- exploreplot(input)
  expect_true(grepl("!duplicated", out))
  
  # change subset
  input$subset <- "TIME > 0"
  out <- exploreplot(input)
  expect_true(grepl("subset", out))
  
})
