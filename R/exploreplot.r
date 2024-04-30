#------------------------------------------ exploreplot ------------------------------------------
#' Function to create plot from data exploration app
#'
#' This function creates a text string for a ggplot based on an input list. This function
#' is specifically written to be used with the shiny app for data exploration.
#'
#' @param inputlist list with input items to create a plot
#'
#' @export
#' @return a character string with the ggplot code
#' @author Richard Hooijmaijers
#' @family Plotting functions
#' @examples
#'
#' \dontrun{exploreplot(input)}
exploreplot <- function(inputlist){
  tmp <-unlist(inputlist)
  tmp <- tmp[order(names(tmp))]
  #print(tmp)
  # Checks/validation steps
  if(inputlist$geoms1=='[empty]') stop("Please provide at least a geom for first layer")
  if(inputlist$Yval1=='[empty]' &  inputlist$geoms1%in%c("point","line","boxplot","smooth","jitter","text")) stop("Please provide at least an y value for layer 1")
  if(inputlist$label1=='[empty]' &  inputlist$geoms1=="text") stop("Please provide a label in case of geom_text")
  if((is.na(inputlist$xlim1) &  !is.na(inputlist$xlim2))|(!is.na(inputlist$xlim1) &  is.na(inputlist$xlim2))) stop("In case of setting x-axis provide lower as well as upper limit")
  if((is.na(inputlist$ylim1) &  !is.na(inputlist$ylim2))|(!is.na(inputlist$ylim1) &  is.na(inputlist$ylim2))) stop("In case of setting y-axis provide lower as well as upper limit")
  if((inputlist$Xlog==TRUE &  inputlist$Xfact==TRUE) | inputlist$Ylog==TRUE &  inputlist$Yfact==TRUE) stop("Cannot set log scale in case a factor is used ")

  # take into account that colour is used to map colour/fill and shape is used to map shape and linetype
  inputlist$attrl <- FALSE
  ggstr <- NULL
  if(inputlist$subset=="" & inputlist$nondups=="")  ggstr <-  paste0(ggstr,"\n","ggplot(r$dataIn)")
  if(inputlist$subset=="" & inputlist$nondups!="")  ggstr <-  paste0(ggstr,"\n","ggplot(subset(r$dataIn, !duplicated(",inputlist$nondups,")))")
  if(inputlist$subset!="" & inputlist$nondups=="")  ggstr <-  paste0(ggstr,"\n","ggplot(subset(r$dataIn,",inputlist$subset,"))")
  if(inputlist$subset!="" & inputlist$nondups!="")  ggstr <-  paste0(ggstr,"\n","ggplot(subset(r$dataIn, !duplicated(",inputlist$nondups,") & ",inputlist$subset,"))")

  addlay <- function(ageom,ayval,axval,agroup,acolour,ashape,asize,alabel,astats,afcol,afsize,afalph){
    if(astats!='[empty]' &  ageom%in%c("boxplot","bar","histogram","smooth","jitter","text")) stop("Stats can only be displayed as 'line' or 'point'")

    aess             <- c(x=axval,y=ayval,group=agroup,shape=ashape,colour=acolour,size=asize,label=alabel)
	  ffact            <- ifelse(inputlist$geoms1=="boxplot"|inputlist$geoms2=="boxplot"|inputlist$geoms3=="boxplot",1,0)
	  aess['x']        <- ifelse(inputlist$Xfact==TRUE|ffact,paste0("factor(",aess['x'],")"),aess['x'])
    #aess['x']        <- ifelse(inputlist$Xfact==TRUE,paste0("factor(",aess['x'],")"),aess['x'])
    aess['y']        <- ifelse(inputlist$Yfact==TRUE,paste0("factor(",aess['y'],")"),aess['y'])
	  aess['colour']   <- ifelse((ageom%in%c("bar","smooth","boxplot") | astats!='[empty]') & acolour!='[empty]',paste0("factor(",aess['colour'],")"),aess['colour'])
	  aess['shape']    <- ifelse(ashape!='[empty]' ,paste0("factor(",aess['shape'],")"),aess['shape'])
    aess['linetype'] <- aess['shape']
    aess['fill']     <- aess['colour']
	  aess['colour']   <- ifelse(afcol!='default','[empty]',aess['colour'])
	  aess['size']     <- ifelse(afsize!=1,'[empty]',aess['size'])
    aess             <- aess[aess!='[empty]']

    argm             <- c(stat=' ',identity=' ')
    argm['stat']     <- ifelse(ayval!='[empty]' & ageom=="bar","'identity'",' ')
    argm['position'] <- ifelse(ageom=="bar" & inputlist$stack==FALSE,"'dodge'",' ')

	if(afcol!='default' & ageom!='boxplot')  argm <- c(argm,color=paste0("'",afcol,"'"))
  if(afcol!='default' & ageom=='boxplot')  argm <- c(argm,fill=paste0("'",afcol,"'"))
	if(afsize!=1 )                           argm <- c(argm,size=afsize)
	if(afalph!=1 )                           argm <- c(argm,alpha=afalph)
	if(ageom=='smooth' & inputlist$omitSE)   argm <- c(argm,se=FALSE)

	argm  <- argm[argm!=' ']
  if(astats!="[empty]") argm <- argm[argm!='stat' & argm!='identity']
	argm  <-  paste(paste(names(argm),argm,sep="="),collapse=", ")
	aess  <-  paste(paste(names(aess),aess,sep="="),collapse=", ")

	if(astats%in%c("mean","median")){
	  lay  <- paste0("stat_summary","(aes(",aess,"), fun=",astats,", geom='", ageom,"', ",argm,")")
	}else if(astats=="mean (SD)"){
	  lay  <- paste0("stat_summary","(aes(",aess,"), fun=mean, funmin=function(x) mean(x) - sd(x), funmax=function(x) mean(x) + sd(x), geom='errorbar', width = 0.2, ",argm,")")
	}else if(astats=="median (5-95th perc.)"){
	  lay  <- paste0("stat_summary","(aes(",aess,"), fun=median, funmin=function(x) quantile(x,0.05), funmax=function(x) quantile(x,0.95), geom='errorbar', width = 0.2, ",argm,")")
	}else{
	  lay  <- paste0("geom_",ageom,"(aes(",aess,"), ",argm,")")
	}

    return(lay)
  }

  # Define different layers - taken into account that for layers 2 and 3 it is not necessary to define x and y values (layer 1 values will be used in case empty)
  lay1   <- addlay(inputlist$geoms1,inputlist$Yval1,inputlist$Xval1,inputlist$group1,inputlist$colour1,inputlist$shape1,inputlist$size1,inputlist$label1,inputlist$stats1,inputlist$fcol1,inputlist$fsize1,inputlist$falph1)
  xvals2 <- ifelse(inputlist$geoms2!='[empty]' & inputlist$Xval2=='[empty]' & inputlist$Xval1!='[empty]',inputlist$Xval1,inputlist$Xval2)
  yvals2 <- ifelse(inputlist$geoms2!='[empty]' & inputlist$Yval2=='[empty]' & inputlist$Yval1!='[empty]',inputlist$Yval1,inputlist$Yval2)
  lay2   <- addlay(inputlist$geoms2,yvals2,xvals2,inputlist$group2,inputlist$colour2,inputlist$shape2,inputlist$size2,inputlist$label2,inputlist$stats2,inputlist$fcol2,inputlist$fsize2,inputlist$falph2)
  xvals3 <- ifelse(inputlist$geoms3!='[empty]' & inputlist$Xval3=='[empty]' & inputlist$Xval1!='[empty]',inputlist$Xval1,inputlist$Xval3)
  yvals3 <- ifelse(inputlist$geoms3!='[empty]' & inputlist$Yval3=='[empty]' & inputlist$Yval1!='[empty]',inputlist$Yval1,inputlist$Yval3)
  lay3   <- addlay(inputlist$geoms3,yvals3,xvals3,inputlist$group3,inputlist$colour3,inputlist$shape3,inputlist$size3,inputlist$label3,inputlist$stats3,inputlist$fcol3,inputlist$fsize3,inputlist$falph3)

  # add additional scales, facets, etc
  # FOR LN SCALE CONSIDER (scales package used!): scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) exp(x)),labels = trans_format("log", math_format(e^.x)))
  add <- NULL; fct <- NULL
  #if(inputlist$refyn==TRUE)  add <- c(add,ab=paste0("geom_abline(intercept=",inputlist$refint,", slope=",inputlist$refslope,")"))
  #if(inputlist$vrefyn==TRUE) add <- c(add,vref=paste0("geom_vline(xintercept=",inputlist$vref,")"))
  if(!is.na(inputlist$refint) & !is.na(inputlist$refslope))  add <- c(add,ab=paste0("geom_abline(intercept=",inputlist$refint,", slope=",inputlist$refslope,")"))
  if(!is.na(inputlist$vref))                                 add <- c(add,vref=paste0("geom_vline(xintercept=",inputlist$vref,")"))
  if((!is.na(inputlist$xlim1) | !is.na(inputlist$xlim2)) & is.na(inputlist$ylim1)) add <- c(add,coord=paste0("coord_cartesian(xlim = c(",inputlist$xlim1,",",inputlist$xlim2,"))"))
  if((!is.na(inputlist$ylim1) | !is.na(inputlist$ylim2)) & is.na(inputlist$xlim1)) add <- c(add,coord=paste0("coord_cartesian(ylim = c(",inputlist$ylim1,",",inputlist$ylim2,"))"))
  if((!is.na(inputlist$xlim1) | !is.na(inputlist$xlim2)) & (!is.na(inputlist$ylim1) | !is.na(inputlist$ylim2))){
    add <- c(add,coord=paste0("coord_cartesian(xlim = c(",inputlist$xlim1,",",inputlist$xlim2,"),ylim=c(",inputlist$ylim1,",",inputlist$ylim2,"))"))
  }
  if(inputlist$Xlog==TRUE) add <- c(add,xlog="scale_x_log10()")
  if(inputlist$Ylog==TRUE) add <- c(add,ylog="scale_y_log10()")
  if(inputlist$facet1!='[empty]') fct <- paste0("~",inputlist$facet1)
  if(inputlist$facet1!='[empty]' & inputlist$facet2!='[empty]') fct <- paste0("~",inputlist$facet1,"+",inputlist$facet2)
  if(inputlist$facet1!='[empty]' & inputlist$facet2!='[empty]' & inputlist$facet3!='[empty]') fct <- paste0("~",inputlist$facet1,"+",inputlist$facet2,"+",inputlist$facet3)
  if(is.na(inputlist$ncol)){ncols <- NULL}else{ncols <- inputlist$ncol}
  if(!is.null(fct)) add <- c(add,fac=paste0("facet_wrap(",fct,",scales='",inputlist$facetsc,"', labeller=label_both, ncol=",ncols,")"))
  
  xlb    <- ifelse(inputlist$xlab != "", inputlist$xlab, inputlist$Xval1)
  ylb    <- ifelse(inputlist$ylab != "", inputlist$ylab, inputlist$Yval1)
  if(ylb=="[empty]") ylb <- "Count" # y label could only be empty in case of histogram, otherwise y variable should be selected
  #if(inputlist$xlab!='') add <- c(add,xlab=paste0("xlab('",inputlist$xlab,"')"))
  #if(inputlist$ylab!='') add <- c(add,ylab=paste0("ylab('",inputlist$ylab,"')"))
  add <- c(add,xlab=paste0("xlab('",xlb,"')"))
  add <- c(add,ylab=paste0("ylab('",ylb,"')"))
  if(inputlist$ptitle!='') add <- c(add,ggtitle=paste0("ggtitle('",inputlist$ptitle,"')"))

  # set manual color scale or fill in case one of the layers has colors (AND it is set as factor!!)
  cond1 <- inputlist$colour1!='[empty]' & grepl(paste0("factor\\(r$dataIn.",inputlist$colour1),inputlist$precode)
  cond2 <- inputlist$colour2!='[empty]' & grepl(paste0("factor\\(r$dataIn.",inputlist$colour2),inputlist$precode)
  cond3 <- inputlist$colour3!='[empty]' & grepl(paste0("factor\\(r$dataIn.",inputlist$colour3),inputlist$precode)
  cond4 <- inputlist$colour1!='[empty]' & inputlist$geoms1%in%c("boxplot","bar","histogram")
  cond5 <- inputlist$colour2!='[empty]' & inputlist$geoms2%in%c("boxplot","bar","histogram")
  cond6 <- inputlist$colour3!='[empty]' & inputlist$geoms3%in%c("boxplot","bar","histogram")
  if(cond1 | cond2 | cond3) add <- c(add,custcol="scale_color_manual(values=scales::brewer_pal('qual','Paired')(12))")
  if(cond4 | cond5 | cond6) add <- c(add,custcol="scale_color_manual(values=scales::brewer_pal('qual','Paired')(12)) + scale_fill_manual(values=scales::brewer_pal('qual','Paired')(12))")

  # Create final return string
  finstr <- paste(ggstr,"+\n ",lay1)
  if(inputlist$geoms2!='[empty]') finstr <- paste(finstr,"+\n ",lay2)
  if(inputlist$geoms3!='[empty]') finstr <- paste(finstr,"+\n ",lay3)
  if(!is.null(add)) finstr <- paste(finstr,"+\n ",paste(add,collapse=" + \n  "))
  finstr <- paste0(finstr,"+\n ","shinyMixR::theme_shinyMixR(fontsize=",inputlist$fontsize,")")
  # Add functionality for multipage plotting
  # if(inputlist$npage>1) finstr <- paste0("ggpage(\n",finstr,",\n pages=",inputlist$npage,")")
  if(inputlist$precode!='') finstr <- paste(inputlist$precode,"\n",finstr)
  return(finstr)
}
