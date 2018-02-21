#------------------------------------------ gof_plot ------------------------------------------
#' Create goodness of fit plots
#'
#' Creates goodness of fit plots either using the xpose.nlmixr package or using a
#' default ggplot call
#'
#' @param dfrm data frame as created by the nlmixr function
#' @param type character defining the type of plot that should be created. currently
#'   "xpose" and "user" are supported for xpose or ggplot style of plots
#' @param mdlnm character with name of the model
#' @param outnm character with name of the output file (see details)
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
#'  gof_plot(res)
#' }
gof_plot <- function(dfrm,type="xpose",mdlnm=NULL,outnm=NULL,...){
  if(type=="xpose"){
    xpdb <- xpose.nlmixr::xpose_data_nlmixr(dfrm)
    p1   <- xpose::dv_vs_pred(xpdb)
    p2   <- xpose::dv_vs_ipred(xpdb)
    p3   <- xpose::res_vs_pred(xpdb)
    p4   <- xpose::res_vs_idv(xpdb)
  }else if(type=="user"){
    p1  <- ggplot(dfrm,aes(DV,PRED)) + geom_point(alpha=.6)  + geom_abline(intercept=0,slope=1,colour="darkblue",linetype=2)
    p2  <- ggplot(dfrm,aes(DV,IPRED)) + geom_point(alpha=.6) + geom_abline(intercept=0,slope=1,colour="darkblue",linetype=2)
    p3  <- ggplot(dfrm,aes(TIME,CWRES)) + geom_point(alpha=.6) + geom_hline(yintercept=0,colour="darkblue",linetype=2)
    p4  <- ggplot(dfrm,aes(PRED,CWRES)) + geom_point(alpha=.6) + geom_hline(yintercept=0,colour="darkblue",linetype=2)
  }
  if(is.null(outnm)){
    gridExtra::grid.arrange(p1+ggtitle("A"),p2+ggtitle("B"),p3+ggtitle("C"),p4+ggtitle("D"))
  }else{
    if(is.null(mdlnm)) stop("in case output should be saved, mdlnm should be given")
    dir.create(paste0("./analysis/",mdlnm),showWarnings=FALSE)
    if(grepl("\\.tex$",outnm)){
      R3port::ltx_plot(gridExtra::grid.arrange(p1+ggtitle("A"),p2+ggtitle("B"),p3+ggtitle("C"),p4+ggtitle("D")),
                       out=paste0("./analysis/",mdlnm,"/",basename(outnm)),...)
    }else if(grepl("\\.html$",outnm)){
      R3port::html_plot(gridExtra::grid.arrange(p1+ggtitle("A"),p2+ggtitle("B"),p3+ggtitle("C"),p4+ggtitle("D")),
                        out=paste0("./analysis/",mdlnm,"/",basename(outnm)),...)
    }
  }
}
