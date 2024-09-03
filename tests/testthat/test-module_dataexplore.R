test_that("Can run a data exploration", {
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = TRUE)
  
  #models <- paste0(temp_dir, "/files", "/models/run1.r")
  
  # scripts <- paste0(temp_dir, "/files", 
  #                   c("/scripts/combined.results.html.r",
  #                     "/scripts/eta.plot.r",
  #                     "/scripts/vpc.plot.r"))
  
  # if shinyMixR/temp directory does not exist yet, create it
  if (!dir.exists(paste0(temp_dir, "/files/shinyMixR/temp"))) {
    dir.create(paste0(temp_dir, "/files/shinyMixR/temp"), recursive = TRUE)
  }
  
  r <- list(
    active_tab = "expl",
    this_wd = paste0(temp_dir, "/files")
  )
  
  testServer(module_dataexplore_server, 
             args = list(r = r), {
               session$setInputs(mdls   = "run1",
                                 Xval1  = "TIME",
                                 Yval1  = "DV",
                                 stats1 = "[empty]",
                                 geoms1 = "point",
                                 fcol1 = "default",
                                 group1 = "ID",
                                 colour1 = "[empty]",
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
               
               suppressWarnings({
                 # Make a new plot
                 session$setInputs(make = 1)
                 
                 # Time for the plot to render
                 Sys.sleep(2)
                 
                 #print(str(createplot()))
                 #print(session$input)
                 
                 # Check if plot object is present 
                 expect_equal(is.ggplot(createplot()), TRUE)
               })
             })
  
  unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
  
}) 