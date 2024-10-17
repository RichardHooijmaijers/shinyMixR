#' Record a shinyMixR test
#' 
#' @param dir character, the directory where the files will be placed and the test will be recorded
#' @param overwrite logical, if TRUE, the function will remove the content of the tests/files directory
#' @param record logical, if TRUE, the function will use `record_test`
#' @param incres logical, if TRUE, the function will include result files for testing
#'
#' @return runs the shinyMixR interface and records actions in the app
#' @keywords internal
setup_shinymixr_test <- function(dir = "./tests/files", overwrite = TRUE, record = FALSE, incres = FALSE) {
  
  if (!file.exists(dir)) {
    dir.create(dir)
  }
  
  # remove content of tests/files directory, if desired
  if (overwrite == TRUE && file.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }

  create_proj(dir, overwrite = overwrite)
  
  if(incres){
    file.copy(system.file(c("other/run1.res.rds","other/run1.ressum.rds"),package="shinyMixR"),
              paste0(dir,"/shinyMixR"))
  }
  
  # create .Rprofile file to store settings
  # if (!file.exists(paste0(dir, "/.Rprofile"))) {
  #   writeLines("options(shiny.testmode = TRUE)", 
  #              con = paste0(dir, "/shinyMixR/app/.Rprofile"))
  # }
  
  if (record == TRUE) {
    shinytest2::record_test(paste0(dir, "/shinyMixR/app"))
  }
}
#------------------------------------------ numfmt ------------------------------------------
#' set significant digits without rounding higher numbers
#'
#' This function sets significant digits without rounding any numbers
#' @param x a numerical vector
#' @param sdig a single number defining the number of significant digits
#' @export
#' @return a character vector with formatted numbers
#' @author Richard Hooijmaijers
#' @examples
#' numfmt(c(0.012,12345,1))
numfmt <- function(x,sdig=3){
  om <- floor(log10(abs(x)))
  dp <- sdig-om-1
  dp <- ifelse(dp<0 | is.na(dp) | is.infinite(dp),0,dp)
  sprintf(paste("%.",dp,"f", sep=""), x)
}