#------------------------------------------ create_proj ------------------------------------------
#' Creates a new project
#'
#' Creates a new project which basically means that within the specified folder,
#' the necessary folder structure will be created and some example models will be placed in it.
#'
#' @param loc character with the location where the project should be created
#' @param overwrite logical indicating if files should be overwritten if already exists
#'
#' @return nothing will be returned by the function (only system commands are issued)
#' @author Richard Hooijmaijers
#' @export
#' @examples
#'
#' \dontrun{
#'  create_proj()
#' }
create_proj <- function(loc=".", overwrite=FALSE){
  
  # we want to create subfolders directly in loc, check run_app functionality to see changes there
  # for this PR we can save the app in loc/shinyMixR/app, in the end we will not need this
  # loc <- paste0(loc, "/shinyMixR/app")
  
  if(!dir.exists(loc)) dir.create(loc, recursive = TRUE)
  
  # First create the folder structure
  dirs <- paste0(loc, c("/analysis","/data","/models","/shinyMixR","/scripts"))
  if(!all(dirs%in%list.files(loc,full.names = TRUE))){
    sapply(dirs,dir.create,showWarnings = FALSE,recursive=TRUE)
    # Now place in some default models and data
    if(!file.exists(paste0(loc,"/models/run1.r")) | overwrite)      file.copy(paste0(system.file(package = "shinyMixR"),"/other/run1.r"),paste0(loc,"/models/run1.r"))
    if(!file.exists(paste0(loc,"/data/theo_sd.rds")) | overwrite)   file.copy(paste0(system.file(package = "shinyMixR"),"/other/theo_sd.rds"),paste0(loc,"/data/theo_sd.rds"))
    if(!file.exists(paste0(loc,"/scripts/eta.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/eta.plot.r"),paste0(loc,"/scripts/eta.plot.r"))
    if(!file.exists(paste0(loc,"/scripts/vpc.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/vpc.plot.r"),paste0(loc,"/scripts/vpc.plot.r"))
    if(!file.exists(paste0(loc,"/scripts/combined.results.html.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/combined.results.html.r"),paste0(loc,"/scripts/combined.results.html.r"))
  }
}
