#------------------------------------------ run_shinymixr ------------------------------------------
#' Runs the shiny app for nlmixr
#'
#' @param ... arguments passed to the shiny runApp function
#'
#' @export
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_shinymixr()
#' }
run_shinymixr <- function(...){
  assign(".cwd",getwd(),pos = .GlobalEnv)
  shiny::runApp(system.file("Dashboard",package="shinyMixR"),...)
}
