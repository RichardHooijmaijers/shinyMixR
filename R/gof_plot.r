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
#' @param colby character vector of length one specifying the variable to color on (for now can be only one variable)
#' @param ptype The type of plots to create. Currently the following is accepted:
#'   "all", "ipred.dv", "pred.dv", "idv.res", "pred.res"
#' @param outnm character with name of the output file (see details)
#' @param projloc character with the base location of the shinyMixR project
#' @param title character with the title to place above the plot
#' @param linscale Logical indicating if the scales should be set to linear for DV, PRED and IPRED plots
#' @param ... additional arguments passed to \code{\link[R3port]{ltx_plot}} or \code{\link[R3port]{html_plot}}
#'
#' @details In case a model is saved, a directory with the name of the model is created within the
#'   analysis folder of the current project. Then within this folder the file is saved as outnm.
#'   This method was chosen so the interface can easily index applicable files for a certain model.
#'   However, this means that output is always saved in this directly regardless of the location of outnm
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
gof_plot <- function(dfrm,type="xpose",mdlnm=NULL,colby=NULL,ptype="all",outnm=NULL,projloc=".",title=NULL,linscale=FALSE,...){
  if(type=="xpose" & !is.null(colby)) stop("Color by does not work with xpose type of plots")
  if(type=="xpose" && length(find.package("nlmixr2est", quiet = TRUE))>0){
    dat <- xpose.nlmixr2::xpose_data_nlmixr2(dfrm)
  }else{
    dat <- as.data.frame(dfrm)
  }
  plfun <- function(data,plottype){
    if(type=="xpose"){
      # types are "all", "ipred.dv", "pred.dv", "idv.res", "pred.res" .
      if(plottype=="pred.dv")  pl <- xpose::dv_vs_pred(data,title=NULL,subtitle=NULL,caption=NULL)
      if(plottype=="ipred.dv") pl <- xpose::dv_vs_ipred(data,title=NULL,subtitle=NULL,caption=NULL)
      if(plottype=="pred.res") pl <- xpose::res_vs_pred(dat,res=ifelse(is.null(dfrm$CWRES),"RES","CWRES"),title=NULL,subtitle=NULL,caption=NULL)
      if(plottype=="idv.res")  pl <- xpose::res_vs_idv(dat,res=ifelse(is.null(dfrm$CWRES),"RES","CWRES"),title=NULL,subtitle=NULL,caption=NULL)
    }else if(type=="user"){
      if(!is.null(colby)) data[,colby] <- as.factor(data[,colby])
      if(plottype=="pred.dv")  pl <- ggplot(data,aes_string("DV","PRED"))
      if(plottype=="ipred.dv") pl <- ggplot(data,aes_string("DV","IPRED"))
      if(plottype=="pred.res") pl <- ggplot(data,aes_string("PRED",ifelse(is.null(dfrm$CWRES),"RES","CWRES")))
      if(plottype=="idv.res")  pl <- ggplot(data,aes_string("TIME",ifelse(is.null(dfrm$CWRES),"RES","CWRES")))
      if(!is.null(colby)){
        pl <- pl + geom_point(alpha=.6,aes_string(color=colby))
      }else{
        pl <- pl + geom_point(alpha=.6)
      }
      slp <- ifelse(plottype%in%c("ipred.dv","pred.dv"),1,0)
      pl  <- pl + geom_abline(intercept=0,slope=slp,colour="darkblue",linetype=2)
    }
    if(plottype%in%c("pred.dv","ipred.dv")){
      limt <- ifelse(plottype=="pred.dv","PRED","IPRED")
      lims <- c(as.data.frame(dfrm)[,"DV"],as.data.frame(dfrm)[,limt])
      if(!linscale) lims <- range(lims[lims>0]) else lims <- range(lims)
      pl  <- pl + coord_cartesian(xlim=lims,ylim=lims)
      if(!linscale) pl <- pl + scale_y_log10() + scale_x_log10() + annotation_logticks(sides = "bl")
    }
    pl <- pl + theme_shinyMixR()
    return(pl)
  }
  if(ptype=="all"){
    p1 <- plfun(dat,"pred.dv"); p2 <- plfun(dat,"ipred.dv"); p3 <- plfun(dat,"pred.res"); p4 <- plfun(dat,"idv.res");
    pl <- patchwork::wrap_plots(p1,p2,p3,p4,guides="collect") & theme(legend.position="bottom")
    pl <- pl <- pl + patchwork::plot_annotation(tag_levels = 'A')
    pl <- pl + patchwork::plot_annotation(title = title)
  }else{
    pl <- plfun(dat,ptype)
    if(!is.null(title)) pl <- pl + labs(title=title)
  }

  if(is.null(outnm)){
    return(pl)
  }else{
    if(is.null(mdlnm)) stop("in case output should be saved, mdlnm should be given")
    dir.create(paste0(projloc,"/analysis/"),showWarnings=FALSE)
    dir.create(paste0(projloc,"/analysis/",mdlnm),showWarnings=FALSE)
    if(grepl("\\.tex$",outnm)){
      R3port::ltx_plot(pl,out=paste0(projloc,"/analysis/",mdlnm,"/",basename(outnm)),title="GOF plots",...)
    }else if(grepl("\\.html$",outnm)){
      R3port::html_plot(pl,out=paste0(projloc,"/analysis/",mdlnm,"/",basename(outnm)),title="GOF plots",...)
    }
  }
}
