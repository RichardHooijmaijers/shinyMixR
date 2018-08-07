# Script to create histogram of eta values
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
library(ggplot2)
lapply(models,function(x){
  dat  <- readRDS(paste0("./shinyMixR/",x,".res.rds"))
  ## MLF: this is error prone since you don't have to name an eta eta.cl for instance...
  ## dat  <- dat[!duplicated(dat$ID),]
  ## etav <- names(dat)[grep("eta\\.",names(dat))]
  etav <- nlme::ranef(dat)[, -1];
  if(length(etav)!=0){
    pll <- lapply(etav,function(pl){
      ggplot(dat,aes_string(pl)) + geom_histogram(fill="grey",color="black") + labs(title=pl)
    })
    dir.create(paste0("./analysis/",x),showWarnings=FALSE)
    R3port::html_plot(pll,out=paste0("./analysis/",x,"/hist.eta.html"),show=FALSE)
  }
})
