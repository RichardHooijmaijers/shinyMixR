# Script to create VPC
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
library(ggplot2)
lapply(files,function(x){
  mdln  <- tools::file_path_sans_ext(basename(x))
  rootl <- normalizePath(paste0(dirname(x),"/../"),winslash = "/")
  res   <- readRDS(paste0(rootl,"/shinyMixR/",mdln,".res.rds"))
  dir.create(paste0(rootl,"/analysis/",mdln),showWarnings=FALSE)
  if(length(find.package("nlmixr2", quiet = TRUE))>0){
    try(R3port::html_plot(nlmixr2plot::vpcPlot(res,n=500,show=list(obs_dv=TRUE)),out=paste0(rootl,"/analysis/",mdln,"/vpc.plot.html"),show=FALSE,title="VPC"))
  }else{
    try(R3port::html_plot(nlmixr::vpc(res,nsim=500,show=list(obs_dv=TRUE)),out=paste0(rootl,"/analysis/",mdln,"/vpc.plot.html"),show=FALSE,title="VPC"))
  }
})
message("Script done!\n")