test_that("run_shinymixr returns app object", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Don't run this test on CI
  skip_on_ci()
  
  app <- suppressWarnings(run_shinymixr())
  
  expect_is(app, "shiny.appobj")
  
  # delete created shinyMixR/ subdirectory
  unlink("shinyMixR", recursive = TRUE)
  
})