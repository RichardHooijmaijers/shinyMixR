#------------------------------------------ create_proj ------------------------------------------
#' Creates a new project
#'
#' Creates a new project which basically means that within the specified folder,
#' the necessary folder structure will be created and some example models will be placed in it.
#'
#' @param loc character with the location where the project should be created
#'
#' @return nothing will be returned by the function (only system commands are issued)
#' @author Richard Hooijmaijers
#' @export
#' @examples
#'
#' \dontrun{
#'  create_proj()
#' }
create_proj <- function(loc="."){
  # First create the folder structure
  dirs <- paste0(loc,c("/analysis","/data","/models","/shinyMixR","/scripts"))
  sapply(dirs,dir.create,showWarnings = FALSE)
  # Now place in some default models and data
  file.copy(paste0(system.file(package = "shinyMixR"),"/Other/run1.r"),paste0(loc,"/models/run1.r"))
  file.copy(paste0(system.file(package = "shinyMixR"),"/Other/run2.r"),paste0(loc,"/models/run2.r"))
  file.copy(paste0(system.file(package = "shinyMixR"),"/Other/theo_sd.rds"),paste0(loc,"/data/theo_sd.rds"))
  file.copy(paste0(system.file(package = "shinyMixR"),"/Other/eta.plot.r"),paste0(loc,"/scripts/eta.plot.r"))
}
