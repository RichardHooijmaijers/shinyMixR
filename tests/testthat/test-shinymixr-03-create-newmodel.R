library(shinytest2)

test_that("Shiny app correctly creates new model code", {

  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  temp_dir <- tempdir()

  # Set up necessary files (internal function) - include results so we do not need to run models
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"),
                                   overwrite = TRUE,
                                   record = FALSE,
                                   incres = TRUE)

  # Start driver for Shiny test
  shiny_app <- shinyMixR::run_shinymixr(paste0(tempdir(),"/files"))
  app <- AppDriver$new(app_dir = shiny_app,
                       name = "run3-model",
                       seed = 123)

  app$set_inputs(tabs = "editor")
  app$click("editor-newmdl")

  # Capture state for debugging - do we need these intermediate steps?
  app$expect_values(input = "editor-newmdl")

  # Wait for modal to open
  Sys.sleep(1)
  app$click("editor-newgo")

  # Capture state for debugging - do we need these intermediate steps?
  app$expect_values(input = "editor-newgo")

  # Check if new model is created and contains correct naming
  modmade <- "run2.r" %in% list.files(paste0(temp_dir, "/files/models"))
  expect_true(modmade)
  if(modmade){
    modcont <- readLines(paste0(temp_dir, "/files/models/run2.r"))
    expect_true(grepl("run2 <- function", modcont[1]))
  }

  # Perform test to save model with new name (be aware that save-as btn is a module as well)
  app$click("editor-adapt_meta_ed-go")
  # Wait for modal to open
  Sys.sleep(1)
  curvals <- app$get_values()
  expect_true(curvals$input$`editor-adapt_meta_ed-mdladpt`=="run3.r")
  expect_true(curvals$input$`editor-adapt_meta_ed-mdldesc`=="template models")
  expect_true(curvals$input$`editor-adapt_meta_ed-mdlimp`==1)
  app$click("editor-adapt_meta_ed-adpt")
  app$click(selector = ".swal2-confirm")
  expect_true("run3.r"%in%list.files(paste0(tempdir(),"/files/models")))

  # Finally test if update inits works as expected (e.g. are initial changed, values itself tested outside shinytest)
  app$set_inputs(`editor-editLst` = "run1")
  app$click("editor-updinit")
  Sys.sleep(1)
  app$set_inputs("editor-tosave" = c("run99.r"))
  app$click("editor-goupdate",timeout_=120000)
  Sys.sleep(1)
  app$click(selector = ".swal2-confirm")

  # Left out testing of this part for r cmd check/covr
  # omod  <- readLines(paste0(tempdir(),"/files/models/run1.r"))
  # amod  <- readLines(paste0(tempdir(),"/files/models/run99.r"))
  # ores  <- eval(parse(text=c("nlmixr2est::nlmixr(",omod,")$ini")))
  # ares  <- eval(parse(text=c("nlmixr2est::nlmixr(",amod,")$ini")))
  # expect_true(any(!(ores$est==ares$est)))

  # Stop and clean up
  app$stop()
  #unlink(paste0(temp_dir, "/files"), recursive = TRUE)
  rml  <- list.files(tempdir(), recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  unlink(rml, recursive = TRUE)
})
