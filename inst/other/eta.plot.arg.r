# Script to create histogram of eta values
# ShinyMixR will add a vector with the selected models, e.g.:
# models <- c("run1","run2")
#inp# selectInput(ns("histcol"),label="Choose histogram color",choices=c("grey","red","blue"))
library(ggplot2)
lapply(files,function(x){
  mdln  <- tools::file_path_sans_ext(basename(x))
  rootl <- normalizePath(paste0(dirname(x),"/../"),winslash = "/")  
  dat  <- readRDS(paste0(rootl,"/shinyMixR/",mdln,".res.rds"))
  etav <- nlme::ranef(dat)[, -1,drop=FALSE];
  if(length(names(etav))!=0){
    pll <- lapply(names(etav),function(pl){
      ggplot(etav,aes_string(pl)) + geom_histogram(fill=histcol,color="black") + labs(title=pl)
    })
    dir.create(paste0(rootl,"/analysis/",mdln),showWarnings=FALSE)
    R3port::html_plot(pll,out=paste0(rootl,"/analysis/",mdln,"/hist.eta.html"),show=FALSE,title="ETA distribution")
  }
})
cat("Script done!\n")