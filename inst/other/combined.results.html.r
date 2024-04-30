#files <- c("G:/Computerised systems/R/Packages/shinyMixR/Testing/Appwithmodules/dummy project 1/models/run1.r")
# Script to create combined results
library(xpose.nlmixr2)
library(shinyMixR)
lapply(files,function(x){
  mdln  <- tools::file_path_sans_ext(basename(x))
  rootl <- normalizePath(paste0(dirname(x),"/../"),winslash = "/")
  dir.create(paste0(rootl,"/analysis/",mdln),showWarnings=FALSE)
  res      <- readRDS(paste0(rootl,"/shinyMixR/",mdln,".res.rds"))
  ress     <- readRDS(paste0(rootl,"/shinyMixR/",mdln,".ressum.rds"))
  xpdb     <- xpose_data_nlmixr(res)

  ptbl     <- cbind(Parameter=row.names(ress$partblf),ress$partblf)
  R3port::html_list(ptbl,porder=FALSE,title="Parameter table",out=paste0(rootl,"/analysis/",mdln,"/01partable.html"),show=FALSE)

  gof_plot(res, outnm=paste0(rootl,"/analysis/",mdln,"/02gofall.html"),mdlnm=mdln,show=FALSE)
  fit_plot(res, outnm=paste0(rootl,"/analysis/",mdln,"/03indfit.html"),mdlnm=mdln,show=FALSE)
  if(!is.null(xpdb$files)) pl1 <- prm_vs_iteration(xpdb) else pl1 <- list()
  pl2 <- absval_res_vs_idv(xpdb, res = 'IWRES')
  pl3 <- absval_res_vs_pred(xpdb, res = 'IWRES')
  pl4 <- res_distrib(xpdb)
  R3port::html_plot(list(pl1,pl2,pl3,pl4),out=paste0(rootl,"/analysis/",mdln,"/04other.html"),show=FALSE,title="other plots")

  etav <- nlme::ranef(res)[, -1,drop=FALSE];
  if(length(names(etav))!=0){
    pll <- lapply(names(etav),function(pl){
      ggplot(etav,aes_string(pl)) + geom_histogram(fill="grey",color="black") + labs(title=pl)
    })
    R3port::html_plot(pll,out=paste0(rootl,"/analysis/",mdln,"/05hist.eta.html"),show=FALSE,title="ETA distribution")
  }
  try(R3port::html_plot(nlmixr2::vpcPlot(res,n=500,show=list(obs_dv=TRUE)),out=paste0(rootl,"/analysis/",mdln,"/06vpc.plot.html"),show=FALSE,title="VPC"))

  R3port::html_combine(combine=paste0(rootl,"/analysis/",mdln),out="report.html",show=TRUE,
                       template=paste0(system.file(package="R3port"),"/bootstrap.html"),toctheme=TRUE)
})
cat("Script done!\n")