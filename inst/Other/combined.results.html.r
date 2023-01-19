# Script to create combined results
library(xpose.nlmixr2)
library(shinyMixR)
lapply(models,function(x){

  dir.create(paste0("./analysis/",x),showWarnings=FALSE)
  res      <- readRDS(paste0("./shinyMixR/",x,".res.rds"))
  ress     <- readRDS(paste0("./shinyMixR/",x,".ressum.rds"))
  xpdb     <- xpose_data_nlmixr(res)

  ptbl     <- cbind(Parameter=row.names(ress$partblf),ress$partblf)
  R3port::html_list(ptbl,porder=FALSE,title="Parameter table",out=paste0("./analysis/",x,"/01partable.html"),show=FALSE)

  gof_plot(res, outnm=paste0("./analysis/",x,"/02gofall.html"),mdlnm=x,show=FALSE)
  fit_plot(res, outnm=paste0("./analysis/",x,"/03indfit.html"),mdlnm=x,show=FALSE)
  if(!is.null(xpdb$files)) pl1 <- prm_vs_iteration(xpdb) else pl1 <- list()
  pl2 <- absval_res_vs_idv(xpdb, res = 'IWRES')
  pl3 <- absval_res_vs_pred(xpdb, res = 'IWRES')
  pl4 <- res_distrib(xpdb)
  R3port::html_plot(list(pl1,pl2,pl3,pl4),out=paste0("./analysis/",x,"/04other.html"),show=FALSE,title="other plots")

  etav <- nlme::ranef(res)[, -1];
  if(length(names(etav))!=0){
    pll <- lapply(names(etav),function(pl){
      ggplot(etav,aes_string(pl)) + geom_histogram(fill="grey",color="black") + labs(title=pl)
    })
    R3port::html_plot(pll,out=paste0("./analysis/",x,"/05hist.eta.html"),show=FALSE,title="ETA distribution")
  }
  R3port::html_plot(nlmixr::vpcPlot(res,n=500,show=list(obs_dv=TRUE)),out=paste0("./analysis/",x,"/06vpc.plot.html"),show=FALSE,title="VPC")

  R3port::html_combine(combine=paste0("./analysis/",x),out="report.html",show=TRUE,
                       template=paste0(system.file(package="R3port"),"/bootstrap.html"),toctheme=TRUE)
})
