#------------------------------------------ run_shinymixr ------------------------------------------
#' Runs the shiny app for nlmixr
#'
#' @param wd character with the working directory to start from (wd will be set to this location)
#' @param ... arguments passed to the shiny runApp function
#'
#' @export
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_shinymixr()
#' }
run_shinymixr <- function(wd=getwd(),...){
  if(!file.exists(wd)) stop("the wd that was provided does not exist")
  assign(".cwd",normalizePath(wd),pos = .GlobalEnv)
  shiny::runApp(system.file("Dashboard",package="shinyMixR"),...)
}
