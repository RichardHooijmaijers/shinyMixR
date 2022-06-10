#------------------------------------------ fit_plot ------------------------------------------
#' Create fit plot
#'
#' Creates a fit plot either using the xpose.nlmixr package or using a
#' default ggplot call
#'
#' @param dfrm data frame as created by the nlmixr function
#' @param type character defining the type of plot that should be created. currently
#'   "xpose" and "user" are supported for xpose or ggplot style of plots
#' @param mdlnm character with name of the model
#' @param outnm character with name of the output file (see details)
#' @param projloc character with the base location of the shinyMixR project
#' @param ... additional arguments passed to \code{\link[R3port]{ltx_plot}} or \code{\link[R3port]{html_plot}}
#'
#' @details In case a model is saved, a directory with the name of the model is created within the
#'   analysis folder of the current project. Then within this folder the file is saved as outnm.
#'   This method was chosen so the interface can easily index applicable files for a certain model.
#'   However, this means that output is alwasy saved in this directly regardless of the location of outnm
#'
#' @export
#' @return in case no outnm is defined a ggplot object will be returned otherwise
#'   the results are saved to disk
#'
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  fit_plot(res)
#' }
fit_plot <- function(dfrm,type="xpose",mdlnm=NULL,outnm=NULL,projloc=".",...){
  if(type=="xpose"){
    if("nlmixr2" %in% rownames(installed.packages())){
      xpdb <- xpose.nlmixr2::xpose_data_nlmixr2(dfrm)
    }else{
      xpdb <- xpose.nlmixr::xpose_data_nlmixr(dfrm)
    }
    pl   <- xpose::ind_plots(xpdb, nrow=3, ncol=4)
  }else if(type=="user"){
    pl   <- ggplot(dfrm,aes(x=TIME)) + geom_point(aes(y=DV)) +
              geom_line(aes(y=PRED)) + geom_line(aes(y=IPRED),linetype=2) +
              facet_wrap(~ID)
  }
  if(is.null(outnm)){
    pl
  }else{
    if(is.null(mdlnm)) stop("in case output should be saved, mdlnm should be given")
    dir.create(paste0(projloc,"/analysis/",mdlnm),showWarnings=FALSE)
    if(grepl("\\.tex$",outnm)) R3port::ltx_plot(pl+labs(title=mdlnm),title="Fit plots",out=paste0(projloc,"/analysis/",mdlnm,"/",basename(outnm)),...)
    if(grepl("\\.html$",outnm)) R3port::html_plot(pl+labs(title=mdlnm),title="Fit plots",out=paste0(projloc,"/analysis/",mdlnm,"/",basename(outnm)),...)
  }
}
