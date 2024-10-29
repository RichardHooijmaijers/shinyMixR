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
#' @param autoupdate logical indicating if the project object should automatically update
#' @param projloc character with the base location of the shinyMixR project
#' @param addcwres logical indicating if CWRES should be added to the output
#' @param addnpde logical indicating if NPDE should be added to the output
#' 
#' @details the meta data is obtained by compiling the model. The dataset, estimation method and control list are
#'   then included in the nlmixr call. Meta data is included in the model function which is comparable with NONMEM.
#'   This method was chosen so that all information to run a model is kept together in one function
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
run_nmx <- function(mod,proj=proj,ext=TRUE,saverds=TRUE,autoupdate=TRUE,projloc=".",addcwres=TRUE,addnpde=TRUE){

  # dnm     <- deparse(substitute(proj))
  # if(autoupdate) assign(dnm,get_proj(projloc=projloc))
  # Source model to obtain meta data (places meta object in env)
  sret <- try(source(proj[[mod]]$model,local=TRUE))
  meta <- suppressMessages(try(eval(parse(text=c("nlmixr2::nlmixr(",readLines(proj[[mod]]$model),")$meta"))),silent=TRUE))
  if(inherits(meta, "try-error") || inherits(sret, "try-error")){
    cat("Error in model syntax please check before running\n")
    if(ext) writeLines(meta, paste0(projloc,"/shinyMixR/temp/",mod,".prog.txt"))
    return()
  }

  if(ext){
    # In this step it is not possible to place the results in proj object (need refresh function/button)
    # Decided to place everything in a temp script, furthermore have to add keep.source as option.
    dir.create(paste0(projloc,"/shinyMixR/temp"),showWarnings = FALSE,recursive = TRUE)

    # Had to deal with control list in a different way because nested lists are not passed correctly in paste
    cntrll <- trimws(readLines(proj[[mod]]$model))
    cntrla <- grepl("^control *=|^control.*<-",cntrll)
    if(any(cntrla)){cntrll <- gsub("^control *=|^control.*<-","",cntrll[cntrla])}else{cntrll <- "list()"}

    tmpl <- readLines(paste0(system.file(package = "shinyMixR"),"/other/run_nmx.tmp"))
    if(is.null(meta$subs)) subs <- "" else subs <- meta$subs
    
    rlst <- list(modelloc=normalizePath(proj[[mod]]$model,winslash = "/",mustWork = FALSE), 
                 data=meta$data, 
                 subs=subs,
                 est=meta$est, 
                 control=cntrll, 
                 saveres=saverds, 
                 modelname=mod,
                 locproj=normalizePath(projloc,winslash = "/"),
                 addcwres=addcwres,
                 addnpde=addnpde)

    tscr <- paste0(projloc,"/shinyMixR/temp/script.",stringi::stri_rand_strings(1,6),".r")
    writeLines(whisker::whisker.render(tmpl,rlst),tscr)
    
    if(Sys.info()['sysname']=="Windows"){
      shell(paste0(R.home("bin"), "/Rscript.exe \"", tscr,  "\" > \"",projloc,"/shinyMixR/temp/",mod,".prog.txt\" 2>&1"),wait=FALSE)
    }else{
      system(paste0(R.home("bin"), "/Rscript \"", tscr,  "\" > \"",projloc,"/shinyMixR/temp/",mod,".prog.txt\" 2>&1"),wait=FALSE)
    }
  }else{
    data <- readRDS(paste0(projloc,"/data/", meta$data, ".rds"))
    if(!is.null(meta$subs) && meta$subs!="") data_nlm <- subset(data,eval(parse(text=(meta$subs)))) else data_nlm <- data
    modres  <- nlmixr2::nlmixr(eval(parse(text=readLines(proj[[mod]]$model))), data_nlm, est=meta$est,control=meta$control,nlmixr2::tableControl(cwres=addcwres, npde=addnpde))
    if("nlmixr2" %in% rownames(installed.packages())){
      ressum  <- list(OBJF=modres$objective,CONDNR=modres$conditionNumberCor,partbl=modres$parFixedDf,partblf=modres$parFixed,omega=modres$omega,tottime=rowSums(modres$time))
    }else{
      ressum  <- list(OBJF=modres$objective,partbl=modres$popDf,partblf=modres$par.fixed,omega=modres$omega,tottime=rowSums(modres$time))
    }
    if(saverds){
      saveRDS(modres,file=paste0(projloc,"/shinyMixR/",mod,".res.rds"))
      saveRDS(ressum,file=paste0(projloc,"/shinyMixR/",mod,".ressum.rds"))
    }
    proj[[mod]]$results <- ressum
    return(modres)
  }
}
