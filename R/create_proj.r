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
#' 
#' @importFrom cli cli_alert_success cli_alert_info
#'
#' @author Richard Hooijmaijers
#' 
#' @export
#' 
#' @examples
#'
#' \dontrun{
#'  create_proj()
#' }
create_proj <- function(loc=".", overwrite=FALSE){
  
  if(!dir.exists(loc)) {
    dir.create(loc, recursive = TRUE)
    cli::cli_alert_success(paste0("Directory ", loc, " created"))
  }
  
  # First create the folder structure
  dirs <- paste0(loc, c("/analysis", "/data", "/models", "/shinyMixR", "/scripts"))
  
  if(!all(dirs %in% list.files(loc, full.names = TRUE))){
    # for each created directory, display a message
    sapply(dirs, function(x){
      if(!dir.exists(x)) {
        dir.create(x, showWarnings = FALSE, recursive=TRUE)
        cli::cli_alert_success(paste0("Directory ", x, " created"))
      }
    })
    
    # Now place in some default models and data
    if(!file.exists(paste0(loc,"/models/run1.r")) | overwrite)      file.copy(paste0(system.file(package = "shinyMixR"),"/other/run1.r"),paste0(loc,"/models/run1.r"))
    if(!file.exists(paste0(loc,"/data/theo_sd.rds")) | overwrite)   file.copy(paste0(system.file(package = "shinyMixR"),"/other/theo_sd.rds"),paste0(loc,"/data/theo_sd.rds"))
    if(!file.exists(paste0(loc,"/scripts/eta.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/eta.plot.r"),paste0(loc,"/scripts/eta.plot.r"))
    if(!file.exists(paste0(loc,"/scripts/vpc.plot.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/vpc.plot.r"),paste0(loc,"/scripts/vpc.plot.r"))
    if(!file.exists(paste0(loc,"/scripts/combined.results.html.r")) | overwrite) file.copy(paste0(system.file(package = "shinyMixR"),"/other/combined.results.html.r"),paste0(loc,"/scripts/combined.results.html.r"))
  } else {
    cli::cli_alert_info("Project structure already exists, no files were overwritten")
  }
  
  # dont return anything
  return(invisible())
}
