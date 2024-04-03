#------------------------------------------ fit_plot ------------------------------------------
#' Create fit plot
#'
#' Creates a fit plot either using the xpose.nlmixr package or using a
#' default ggplot call
#'
#' @param dfrm data frame as created by the nlmixr function
#' @param type character defining the type of plot that should be created. currently
#'   "xpose" and "user" are supported for xpose or ggplot style of plots
#' @param by character vector with variables for facetting
#' @param idv independent variable or x variable
#' @param obs variable with observed data points
#' @param pred variable with predicted data points
#' @param ipred variable with individual predicted data points
#' @param grp variable for grouping (mainly to draw separate lines)
#' @param logy logical if y-axis should be displayed on log scale
#' @param scales character of length one defining the scale parameter of ggplot (e.g. "fixed", "free","free_y",etc)
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
fit_plot <- function(dfrm,type="xpose",by="ID",idv="TIME",obs="DV",pred="PRED",ipred="IPRED",grp="ID",logy=TRUE,scales="fixed",mdlnm=NULL,outnm=NULL,projloc=".",...){
  if(!all(c(idv,obs,pred)%in%names(dfrm)))     stop("Please provide correct 'idv', 'obs', 'pred'")
  if(!is.null(by) && !all(by%in%names(dfrm)))  stop("Please provide correct 'by'")
  if(!is.null(ipred) && !ipred%in%names(dfrm)) stop("Please provide correct 'ipred'")
  
  if(type=="xpose"){
    if((length(by)==1 && by!="ID") || length(by)>1 || idv!="TIME" || obs!="DV" || pred!="PRED" || ipred!="IPRED" || grp!="ID") stop("Changing variables does not work with xpose type of plots")
    if("nlmixr2" %in% rownames(installed.packages())){
      xpdb <- xpose.nlmixr2::xpose_data_nlmixr2(dfrm)
    }else{
      stop("nlmixr2 package is not installed")
    }
    if(logy) tolog <- "y" else tolog <- NULL
    pl   <- xpose::ind_plots(xpdb, nrow=3, ncol=4,log=tolog)
  }else if(type=="user"){
    dfrm <- as.data.frame(dfrm)
    if(!is.null(by)){
      dfrm        <- dfrm[do.call("order", do.call("list",dfrm[,c(by,idv),drop=FALSE])),]
      byvar       <- lapply(by,function(y) paste0(y,":",dfrm[,y]))
      dfrm$byvar  <- do.call(paste,c(byvar,sep=", "))
      dfrm$byvar  <- factor(dfrm$byvar,levels=unique(dfrm$byvar))
    }
    pl   <- ggplot(dfrm,aes_string(x=idv,group=grp)) + geom_point(aes_string(y=obs)) +
      geom_line(aes_string(y=pred)) 
    if(!is.null(ipred))    pl <- pl + geom_line(aes_string(y=ipred),linetype=2)
    if(!is.null(by))       pl <- pl + facet_wrap("byvar",scales=scales)
    if(logy)               pl <- pl + scale_y_log10()
    pl <- pl + labs(caption="symbols: observations, solid line: population predictions, dashed line: individual predictions") + theme_shinyMixR()
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
