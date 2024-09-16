test_that("run_shinymixr returns app object", {
  
  app <- suppressWarnings(run_shinymixr())
  
  expect_is(app, "shiny.appobj")
  
  # delete created shinyMixR/ subdirectory
  unlink("shinyMixR", recursive = TRUE)
  
})