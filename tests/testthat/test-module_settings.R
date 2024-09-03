test_that("Settings are handled correctly", {
  
  temp_dir <- tempdir()
  
  # Set up necessary files (internal function)
  unlink(paste0(temp_dir,"/files"), recursive = TRUE)
  shinyMixR:::setup_shinymixr_test(dir = paste0(temp_dir, "/files"), 
                                   overwrite = TRUE, 
                                   record = FALSE, 
                                   incres = FALSE)
  
  # if shinyMixR/temp directory does not exist yet, create it
  testServer(module_settings_server, 
             expr = {
               
               # Set inputs
               session$setInputs(plott = "user")
               session$setInputs(fontedt = 8)
               
               # Can only checked if these are picked up
               expect_true(session$input$plott=="user")
               expect_true(session$input$fontedt==8)
             })
  
  unlink(paste0(temp_dir, "/files"), recursive = TRUE) 
  
})
