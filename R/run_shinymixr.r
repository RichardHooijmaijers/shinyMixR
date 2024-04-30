#------------------------------------------ run_shinymixr ------------------------------------------
#' Creates and run the interface
#'
#' @param wd character with the working directory
#' @param dry_run logical, if TRUE, the function will not launch the app, but will only create the necessary files
#' @param ... arguments passed to the shiny runApp function
#' @importFrom shiny runApp
#' @import bs4Dash ggplot2 gridExtra
#' @export
#' @return runs the shinyMixR interface
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_shinymixr(".")
#' }
run_shinymixr <- function(wd = getwd(), dry_run = FALSE, ...){
  
  if(!file.exists(paste0(wd,"/shinyMixR/app/www"))) try(dir.create(paste0(wd,"/shinyMixR/app/www"),recursive = TRUE))
  if(!file.exists(paste0(wd,"/shinyMixR/app/shinyMixR/temp")))    try(dir.create(paste0(wd,"/shinyMixR/app/shinyMixR/temp"),recursive=TRUE))
  
  try(file.copy(system.file("dashboard","app.R",package="shinyMixR"),          paste0(wd,"/shinyMixR/app/app.R"),overwrite = TRUE),silent = TRUE)
  try(file.copy(system.file("dashboard","www/logoshinyMixR.png",package="shinyMixR"), paste0(wd,"/shinyMixR/app/www/logoshinyMixR.png")),silent = TRUE)
  
  # Set the working directory so the project can be found
  adpt <- readLines(system.file("dashboard", "app.R", package = "shinyMixR"))
  adpt <- c(paste0("setwd(\"", normalizePath(wd, winslash = "/"), "\")"), adpt)
  writeLines(adpt, paste0(wd,"/shinyMixR/app/app.R"))
  
  # Clean up stuff before running the app (check if feasible or not)
  try(unlink(list.files(paste0(wd,"/shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE)))
  if (dry_run == TRUE) {
    return()
  } else {
    shiny::runApp(paste0(wd,"/shinyMixR/app"),...)
  }
}
