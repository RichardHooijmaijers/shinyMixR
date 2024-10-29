#------------------------------------------ tree_overview ------------------------------------------
#' Creates tree overview of models
#'
#' Create a graphical collapsible tree overview of the models within a project.
#' This is mostly relevant in case the reference of models is included to
#' visualise the relationship between models
#'
#' @param proj_obj a project object created with \code{\link{get_proj}}
#' @param ... additional arguments passed to \code{\link{overview}}
#'
#' @export
#' @return a data frame is returned with the overview
#' @seealso \code{\link[collapsibleTree]{collapsibleTreeNetwork}} which does most of the work
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  if (interactive()) tree_overview(proj_obj)
#' }
tree_overview <- function(proj_obj, ...){
  tmod     <- overview(proj_obj, ...)
  tmod$ref <- ifelse(tmod$ref==tmod$models,"",tmod$ref)
  stmodn   <- data.frame(from=NA,to="start",imp=0,stringsAsFactors = FALSE)
  if(length(tmod$models[tmod$ref==""])!=0){
    noref  <- data.frame(from="start",to=tmod$models[tmod$ref==""],imp=tmod$imp[tmod$ref==""],stringsAsFactors = FALSE)
  }else{noref <- NULL}
  if(length(tmod$models[tmod$ref!=""])!=0){
    refs   <- data.frame(from=tmod$ref[tmod$ref!=""],to=tmod$models[tmod$ref!=""],imp=tmod$imp[tmod$ref!=""],stringsAsFactors = FALSE)
  }else{refs <- NULL}
  stmodn <- rbind(stmodn,noref,refs)
  #stmodn$imp <- 5-as.numeric(stmodn$imp)
  stmodn$imp      <- ifelse(is.na(stmodn$imp),0,as.numeric(stmodn$imp))
  stmodn$col      <- factor(stmodn$imp,levels=0:5)
  stmodn$from[-1] <- ifelse(!stmodn$from[-1]%in%stmodn$to,"start",stmodn$from[-1])
  levels(stmodn$col) <- c("#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c")
  collapsibleTree::collapsibleTreeNetwork(stmodn, collapsed = FALSE,tooltip=FALSE,zoomable=FALSE,fill="col")
}
