#------------------------------------------ par_table ------------------------------------------
#' Create parameter table
#'
#' Creates a table with the final estimates and percentage CV for all parameters in an nlmixr output
#' file. This can be done for one or multiple models for easy comparison
#'
#' @param proj project object
#' @param models character vector with model names to create table for
#' @param outnm character with name of the output file (see details)
#' @param ... additional arguments passed to \code{\link[R3port]{ltx_plot}} or \code{\link[R3port]{html_plot}}
#'
#' @details In case a model is saved, a directory with the name of the model is created within the
#'   analysis folder of the current project. Then within this folder the file is saved as outnm.
#'   This method was chosen so the interface can easily index applicable files for a certain model.
#'   However, this means that output is alwasy saved in this directly regardless of the location of outnm
#'   In case multiple models are selected the result will be written to the name of the first model in the
#'   models vector.
#'
#' @export
#' @return in case no outnm is defined a ggplot object will be returned otherwise
#'   the results are saved to disk
#'
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  par_table(proj)
#' }
par_table <- function(proj,models,outnm=NULL,...){
  withres <- names(proj)[sapply(proj,function(x) !is.null(x$results))]
  tbls <- lapply(intersect(models,withres),function(x){
    tbl <- proj[[x]]$results$partbl
    Est <- paste0(formatC(tbl$`Est.`,digits=3,width=5,flag=" ")," [",formatC(tbl$`%RSE`,digits=3,width=5,flag=" ")," ]")
    ret <- data.frame(Parameter=row.names(tbl),Est=Est)
    names(ret) <- c("Parameter",x)
    ret
  })
  if(is.null(models) | length(intersect(models,withres))==0 | length(tbls)==0) return(data.frame(Res="no results"))
  res <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Parameter", all = TRUE,sort=FALSE),tbls)
  if(is.null(outnm)){
    res
  }else{
    dir.create(paste0("./analysis/",models[1]),showWarnings=FALSE)
    if(grepl("\\.tex$",outnm)) R3port::ltx_list(res,out=paste0("./analysis/",models[1],"/",basename(outnm)),porder=FALSE,title="Parameter table",...)
    if(grepl("\\.html$",outnm)) R3port::html_list(res,out=paste0("./analysis/",models[1],"/",basename(outnm)),porder=FALSE,title="Parameter table",...)
  }
}
