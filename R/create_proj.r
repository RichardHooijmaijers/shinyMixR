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
  # First create the folder structure
  dirs <- paste0(loc,c("/analysis","/data","/models","/shinyMixR","/scripts"))
  if(!all(dirs%in%list.files(loc,full.names = TRUE))){
    sapply(dirs,dir.create,showWarnings = FALSE)
    # Now place in some default models and data
    if(!file.exists(paste0(loc,"/models/run1.r")) | overwrite)      file.copy(paste0(system.file(package = "shinyMixR"),"/Other/run1.r"),paste0(loc,"/models/run1.r"))
    if(!file.exists(paste0(loc,"/data/theo_sd.rds")) | overwrite)   file.copy(paste0(system.file(package = "shinyMixR"),"/Other/theo_sd.rds"),paste0(loc,"/data/theo_sd.rds"))
    if(!file.exists(paste0(loc,"/scritps/eta.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/Other/eta.plot.r"),paste0(loc,"/scripts/eta.plot.r"))
    if(!file.exists(paste0(loc,"/scritps/vpc.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/Other/vpc.plot.r"),paste0(loc,"/scripts/vpc.plot.r"))
    if(!file.exists(paste0(loc,"/scritps/combined.results.html.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/Other/combined.results.html.r"),paste0(loc,"/scripts/combined.results.html.r"))
  }
}
