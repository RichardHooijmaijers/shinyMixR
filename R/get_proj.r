#------------------------------------------ get_proj ------------------------------------------
#' Read in and update model results in project object
#'
#' This function creates or updates a project object with models and/or results emerged from nlmixr runs
#' A check is performed to see if newer results are present and only updates these.
#'
#' @param moddir character with the directory that includes the model files
#' @param proj character with the rds files that includes the model information
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   proj <- get_proj()
#' }
get_proj <- function(moddir="./models",proj="./shinyMixR/project.rds",datdir="./data",geteval=TRUE){
  # Read in models and place in result objects
  dir.create(dirname(proj),showWarnings = FALSE,recursive = TRUE)
  mdln    <- normalizePath(list.files(moddir,pattern="run[[:digit:]]*\\.[r|R]",full.names = TRUE))
  mdlnb   <- sub("\\.[r|R]","",basename(mdln))
  sumres  <- normalizePath(list.files("shinyMixR",pattern="run[[:digit:]]*\\.ressum\\.rds",full.names = TRUE))
  sumresi <- file.info(sumres)
  summdli <- file.info(mdln)

  # read in data folder (only in case objects are not yet present)
  datf  <- list.files(datdir)
  grepd <- " |^[[:digit:]]|\\!|\\#|\\$|\\%|\\&|\\'|\\(|\\)|\\-|\\;|\\=|\\@|\\[|\\]|\\^\\`\\{\\|\\}"
  if(any(grepl(grepd,datf))) warning("Data files with special characters found, take into acount that models that use these can crash")
  # not relevant to read all data for running nlmixr in separate session (should be loaded in this session!)
  lapply(list.files(datdir,full.names = TRUE),function(x){
    if(!grepl(grepd,x) & !exists(sub("\\.rds$|\\.csv$","",basename(x),ignore.case = TRUE),envir=.GlobalEnv)){
      if(grepl("\\.rds$",x,ignore.case = TRUE)) assign(sub("\\.rds$","",basename(x),ignore.case = TRUE),readRDS(x),pos = .GlobalEnv)
      if(grepl("\\.csv$",x,ignore.case = TRUE)) assign(sub("\\.csv$","",basename(x),ignore.case = TRUE),read.csv(x),pos = .GlobalEnv)
    }
  })

  # Read in models and results
  if(!file.exists(proj)){
    mdls <- lapply(mdln,list)
    names(mdls) <- sub("\\.[r|R]","",basename(mdln))
    if(length(mdln)==0){
      warning("No models present")
    }else{
      for(i in 1:length(mdln)){
        names(mdls[[i]]) <- "model"
        if(geteval) mdls[[i]]$modeleval <- eval(parse(text=c("try(nlmixrUI(",readLines(mdln[i]),"))")))
      }
    }
    for(i in sumres) mdls[[sub("\\.ressum\\.rds","",basename(i))]]$results <- readRDS(i)
    mdls$meta <- list(lastrefresh=Sys.time())
  }else{
    mdls   <- readRDS(proj)
    # for the list with models, check if new models are available or old models are deleted
    # and if models are updated after last refresh:
    # inproj <- unlist(sapply(mdls[names(mdls)[names(mdls)!="meta"]],"[",1))
    inproj <- names(mdls)[names(mdls)!="meta"]
    todel  <- setdiff(tolower(inproj),tolower(mdlnb))
    toadd  <- setdiff(tolower(mdlnb),tolower(inproj))
    if(length(todel)!=0){
      #themods <- sapply(mdls[names(mdls)[names(mdls)!="meta"]],function(x) x$model%in%todel)
      #mdls <- mdls[c(sort(names(themods[!themods])),"meta")]
      mdls <- mdls[c(sort(inproj[!inproj%in%todel]),"meta")]
    }
    if(length(toadd)!=0){
      mdls2 <- lapply(mdln[which(mdlnb%in%toadd)],list)
      #names(mdls2) <- sub("\\.[r|R]","",basename(toadd))
      names(mdls2) <- toadd
      for(i in 1:length(mdls2)){
        names(mdls2[[i]]) <- "model"
        if(geteval) mdls2[[i]]$modeleval <- eval(parse(text=c("try(nlmixrUI(",readLines(mdln[which(mdlnb%in%toadd)][i]),"))")))
      }
      mdls <- c(mdls,mdls2)
    }
    # For model results and meta data, check if there are newer results than time of last save
    if(geteval){
      for(i in mdln){
        if(summdli$mtime[row.names(summdli)==i] > mdls$meta$lastrefresh)
          mdls[[sub("\\.[r|R]","",basename(i))]]$modeleval <- eval(parse(text=c("try(nlmixrUI(",readLines(i),"))")))
      }
    }
    for(i in sumres){
      if(sumresi$mtime[row.names(sumresi)==i] > mdls$meta$lastrefresh)
        mdls[[sub("\\.ressum\\.rds","",basename(i))]]$results <- try(readRDS(i))
    }
    mdls$meta$lastrefresh <- Sys.time()
  }
  # Additional check to see if model is not saved after the last results
  chk       <- data.frame(mdl=sub("\\.[r|R]","",basename(mdln)),mdlsv=summdli$mtime,stringsAsFactors = FALSE)
  chk$ressv <- sumresi$mtime[match(chk$mdl,sub("\\.ressum\\.rds","",basename(sumres)))]
  chk       <- chk[which(chk$mdlsv>chk$ressv),]
  #if(nrow(chk)>0) noret <- apply(chk,1,function(x) cat("Be aware that model is saved after results for",x['mdl'],"\n"))

  saveRDS(mdls,file=proj)
  return(mdls)
}
