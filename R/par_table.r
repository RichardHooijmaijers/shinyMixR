#------------------------------------------ par_table ------------------------------------------
#' Create parameter table
#'
#' Creates a table with the final estimates and percentage CV for all parameters in an nlmixr output
#' file. This can be done for one or multiple models for easy comparison
#'
#' @param proj project object
#' @param models character vector with model names to create table for
#' @param outnm character with name of the output file (see details)
#' @param projloc character with the base location of the shinyMixR project
#' @param bsv logical indicating if between subject variability (BSV) should be added to table
#' @param shrink logical indicating if shrinkage should be added to table
#' @param backt logical indicating if the backtransformed parameters should be returned oposed to the original values
#' @param formatting logical indicating if the formatting should be applied to present the table (not implemented for latex output)
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
#' @return in case no outnm is defined a data frame will be returned otherwise
#'   the results are saved to disk
#'
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  par_table(proj,"run1")
#' }
par_table <- function(proj,models,outnm=NULL,projloc=".",bsv=FALSE,shrink=FALSE,backt=FALSE,formatting=FALSE,...){
  withres <- names(proj)[sapply(proj,function(x) !is.null(x$results))]
  tbls <- lapply(intersect(models,withres),function(x){
    tbl <- proj[[x]]$results$partblf
    if(!backt){
      Est <- paste0(tbl$Est.," [",tbl$`%RSE`,"]")  
    }else{
      if("SE"%in%names(tbl)){
        Est <- ifelse(tbl$SE=="FIXED",paste0(tbl$`Back-transformed(95%CI)`," (FIXED)"),tbl$`Back-transformed(95%CI)`)   
      }else{
        Est <- tbl$`Back-transformed`
      }
    }
    Est <- sub(" \\[\\]","",Est)
    if(bsv){
      if(!shrink)    EstAdd <- ifelse(tbl$`BSV(CV%)`==" ","",paste0("{",tbl$`BSV(CV%)`,"}"))
      if(shrink)     EstAdd <- ifelse(tbl$`BSV(CV%)`==" ","",paste0("{",tbl$`BSV(CV%)`,", ",tbl$`Shrink(SD)%`,"}"))
      if(formatting) EstAdd <- ifelse(EstAdd=="","",paste0("<span style=\"font-size: 0.75em;font-weight: bold;\">",EstAdd,"</span>"))
      Est <- paste(Est,EstAdd)
    }
    ret <- data.frame(Parameter=row.names(tbl),Est=Est)
    if(!is.null(proj[[x]]$results$CONDNR)) CN <- round(proj[[x]]$results$CONDNR,1) else CN <- ""
    ret <- rbind(data.frame(Parameter="OBJF",Est=round(proj[[x]]$results$OBJF,2)),data.frame(Parameter="COND. NR",Est=CN),ret)
    names(ret) <- c("Parameter",x)
    ret
  })
  if(is.null(models) | length(intersect(models,withres))==0 | length(tbls)==0) return(data.frame(Res="no results"))
  res <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Parameter", all = TRUE,sort=FALSE),tbls)
  if(is.null(outnm)){
    res
  }else{
    dir.create(paste0(projloc,"/analysis/",models[1]),showWarnings=FALSE,recursive = TRUE)
    if(grepl("\\.tex$",outnm)) R3port::ltx_list(res,out=paste0(projloc,"/analysis/",models[1],"/",basename(outnm)),porder=FALSE,title="Parameter table",...)
    if(grepl("\\.html$",outnm)) R3port::html_list(res,out=paste0(projloc,"/analysis/",models[1],"/",basename(outnm)),porder=FALSE,title="Parameter table",...)
  }
}
