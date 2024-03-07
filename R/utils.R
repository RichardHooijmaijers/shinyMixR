#' Record a shinyMixR test
#' 
#' @param dir character, the directory where the files will be placed and the test will be recorded
#' @param overwrite logical, if TRUE, the function will remove the content of the tests/files directory
#'
#' @return runs the shinyMixR interface and records actions in the app
#' @keywords internal
setup_shinymixr_test <- function(dir = "./tests/files", overwrite = TRUE, record = FALSE) {
  
  if (!file.exists(dir)) {
    dir.create(dir)
  }
  
  # remove content of tests/files directory, if desired
  if (overwrite == TRUE && file.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }

  create_proj(dir, 
              overwrite = overwrite)
  run_shinymixr(wd = dir, 
                dry_run = TRUE)
  
  # create .Rprofile file to store settings
  if (!file.exists(paste0(dir, "/.Rprofile"))) {
    writeLines("options(shiny.testmode = TRUE)", 
               con = paste0(dir, "/shinyMixR/app/.Rprofile"))
  }
  
  if (record == TRUE) {
    record_test(paste0(dir, "/shinyMixR/app"))
  }
}
