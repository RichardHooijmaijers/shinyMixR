# Script to create VPC
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
library(ggplot2)
lapply(models,function(x){
  res  <- readRDS(paste0("./shinyMixR/",x,".res.rds"))
  dir.create(paste0("./analysis/",x),showWarnings=FALSE)
  R3port::html_plot(nlmixr::vpc(res,nsim=500,show=list(obs_dv=TRUE)),out=paste0("./analysis/",x,"/vpc.plot.html"),show=FALSE,title="VPC")
})
