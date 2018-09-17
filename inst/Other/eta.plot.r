# Script to create histogram of eta values
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
library(ggplot2)
lapply(models,function(x){
  dat  <- readRDS(paste0("./shinyMixR/",x,".res.rds"))
  etav <- nlme::ranef(dat)[, -1];
  if(length(names(etav))!=0){
    pll <- lapply(names(etav),function(pl){
      ggplot(etav,aes_string(pl)) + geom_histogram(fill="grey",color="black") + labs(title=pl)
    })
    dir.create(paste0("./analysis/",x),showWarnings=FALSE)
    R3port::html_plot(pll,out=paste0("./analysis/",x,"/hist.eta.html"),show=FALSE,title="ETA distribution")
  }
})
