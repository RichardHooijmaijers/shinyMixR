#------------------------------------------ run_nmx ------------------------------------------
#' Runs a nlmixr model
#'
#' Runs an nlmixr model from a project object with the possibility to run in an
#' external rsession using a system call (tested within linux only)
#'
#' @param mod character with the model file present in project object
#' @param proj project object
#' @param ext logical indicating if the model should be run external in a separate r session
#' @param saverds logical indicating if the model results should be saved in a rds file
#'
#' @details the meta data is obtained by compiling the model. The dataset, estimation method and control list are
#'   then included in the nlmixr call. Meta data is included in the model function which is comparable with NONMEM.
#'   This method was chosen so that all information to run a model is kept togehter in one function
#'
#' @export
#' @return In case the model is not submitted in a separate R session, the results from nlmixr are returned otherwise
#'   the result of the system call will be returned
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_nmx("run1",proj)
#' }
run_nmx <- function(mod,proj,ext=TRUE,saverds=TRUE){

  dnm     <- deparse(substitute(proj))
  # Source model to obtain meta data (places meta object in env)
  source(proj[[mod]]$model,local=TRUE)
  meta <- eval(parse(text=c("nlmixr(",readLines(proj[[mod]]$model),")$meta")))

  if(ext){
    # In this step it is not possible to place the results in proj object (need refresh function/button)
    # Decided to place everything in a temp script, furthermore have to add keep.source as option.
    dir.create("shinyMixR/temp",showWarnings = FALSE,recursive = TRUE)
    escr <- paste0("library(nlmixr)\n","options(keep.source = TRUE)\n",
                   "source('",normalizePath(proj[[mod]]$model,winslash = "/",mustWork = FALSE),"')\n",
                   "if(!exists(\"",meta$data,"\", envir=.GlobalEnv)) ",meta$data," <- readRDS(\"./data/",meta$data,".rds\")",
                   "\nmodres <- nlmixr(",mod,",",meta$data,",est=\"",meta$est,"\",control=",meta$control,")\n")
    if(saverds) escr <- paste0(escr,"saveRDS(modres,file=\"./shinyMixR/",mod,".res.rds\")\n",
                               "saveRDS(list(OBJF=modres$objective,partbl=modres$par.fixed,omega=modres$omega,",
                               "tottime=rowSums(modres$time)),file=\"./shinyMixR/",
                               mod,".ressum.rds\")")
    tscr <- paste0("./shinyMixR/temp/script.",stringi::stri_rand_strings(1,6),".r")
    writeLines(escr,tscr)
    if(Sys.info()['sysname']=="Windows"){
      shell(paste0("Rscript ", tscr,  " > ./shinyMixR/temp/",mod,".prog.txt 2>&1"),wait=FALSE)
    }else{
      system(paste0("Rscript ", tscr,  " > ./shinyMixR/temp/",mod,".prog.txt 2>&1"),wait=FALSE)
    }
  }else{
    modres  <- nlmixr(eval(parse(text=readLines(proj[[mod]]$model))), get(meta$data), est=meta$est,control=meta$control)
    ressum  <- list(OBJF=modres$objective,partbl=modres$par.fixed,omega=modres$omega,tottime=rowSums(modres$time))
    if(saverds){
      saveRDS(modres,file=paste0("./shinyMixR/",mod,".res.rds"))
      saveRDS(ressum,file=paste0("./shinyMixR/",mod,".ressum.rds"))
    }
    proj[[mod]]$results <- ressum
    assign(dnm,proj,pos = .GlobalEnv)
    return(modres)
  }
}
