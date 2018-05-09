# Script to create histogram of eta values
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
library(ggplot2)
lapply(models,function(x){
  res  <- readRDS(paste0("./shinyMixR/",x,".res.rds"))
  pl   <- nlmixr::vpc(res,nsim=500,show=list(obs_dv=T))
  dir.create(paste0("./analysis/",x),showWarnings=FALSE)
  R3port::html_plot(pl,out=paste0("./analysis/",x,"/vpc.plot.html"),show=FALSE)
})
