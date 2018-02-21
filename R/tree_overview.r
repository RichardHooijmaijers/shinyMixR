#------------------------------------------ tree_overview ------------------------------------------
#' Creates tree overview of models
#'
#' Create a graphical collapsible tree overview of the models within a project.
#' This is mostly relevant in case the reference of models is included to
#' visualise the relationship between models
#'
#' @param ... additional arguments passed to \code{\link{overview}}
#'
#' @export
#' @return a data frame is returned with the overview
#' @seealso \code{\link[collapsibleTree]{collapsibleTreeNetwork}} which does most of the work
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  tree_overview()
#' }
tree_overview <- function(...){
  tmod   <- overview(...)
  stmodn <- data.frame(from=NA,to="start",imp=0,stringsAsFactors = FALSE)
  noref  <- data.frame(from="start",to=tmod$models[tmod$ref==""],imp=tmod$imp[tmod$ref==""],stringsAsFactors = FALSE)
  refs   <- data.frame(from=tmod$ref[tmod$ref!=""],to=tmod$models[tmod$ref!=""],imp=tmod$imp[tmod$ref!=""],stringsAsFactors = FALSE)
  stmodn <- rbind(stmodn,noref,refs)
  #stmodn$imp <- 5-as.numeric(stmodn$imp)
  stmodn$imp <- as.numeric(stmodn$imp)
  stmodn$col <- factor(stmodn$imp,levels=0:5)
  levels(stmodn$col) <- c("#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c")
  collapsibleTree::collapsibleTreeNetwork(stmodn, collapsed = FALSE,tooltip=FALSE,zoomable=FALSE,nodeSize="imp",fill="col")
}
