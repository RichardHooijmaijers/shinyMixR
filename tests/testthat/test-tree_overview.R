test_that("tree overview works as expected", {
  
  # create project
  shinyMixR:::setup_shinymixr_test(dir = paste0(tempdir(),"/files"),
                                   overwrite = TRUE, 
                                   record = FALSE)
  
  # get project object
  proj <- get_proj(projloc = paste0(tempdir(), "/files"))
  
  # generate tree overview
  treeview <- tree_overview(proj)
  
  # tests
  expect_equal(length(treeview), 8)
  expect_equal(class(treeview), c("collapsibleTree", "htmlwidget"))
  expect_equal(treeview$x$data$children[[1]]$name, "run1")
  
  # remove project
  #unlink(paste0(tempdir(), "/files"), recursive = TRUE)
  rml  <- list.files(tempdir(), recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
  
})