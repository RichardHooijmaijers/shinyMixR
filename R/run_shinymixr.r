#------------------------------------------ run_shinymixr ------------------------------------------
#' Creates and run the interface
#'
#' @param widgets character vector with the names of the widget scripts that should be included
#' @param wd character with the working directory
#' @param outloc location of the output shiny app files
#' @param init external file with code for server initialization
#' @param runapp logical indicating if the created shiny app should be run
#' @param ... arguments passed to the shiny runApp function
#'
#' @details The widget script should follow some basic rules. At least a list should be available (info) that
#'   contains information regarding the name, title and icon used for the widget. Furthermore a function defining
#'   the ui as taglist (widgetui) and a function defining the server logic (widgetserver) should be available.
#'   An example of a widgetscript can be found in the package documentation
#'
#' @export
#' @return will create necessary files and if indicated run a shiny application
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_shinymixr(c("widget1.r","widget2.r"))
#' }
run_shinymixr <- function(widgets=getwidg()$widgets,wd=getwd(),outloc=tempdir(),init=getwidg()$init,runapp=TRUE,...){

  # General error checking
  chk <- lapply(widgets,function(x){
    source(x,local=TRUE)
    if(!all(c("info","widgetui","widgetserver")%in%ls())) stop(paste("widget",x,"does not include 'info', 'widgetui' and/or 'widgetserver'"))
    if(!all(c("name","title","icon")%in%names(info)))     stop(paste("info in widget",x,"does not include 'name', 'title' and/or 'icon'"))
    return(unlist(info))
  })
  allnfo <- data.frame(do.call(rbind,chk))
  if(any(duplicated(allnfo$name))) stop("make sure that names of widgets are unique (check names in info list)")

  # create ui/server
  allitems <- lapply(widgets,function(x){
    source(x,local=TRUE)
    tabs  <- paste0("shinydashboard::menuItem('",info$title,"', tabName='",info$name,"', icon=icon('",info$icon,"'))")
    items <- paste0("shinydashboard::tabItem(tabName='",info$name,"',",info$name,"funcUI())")
    uifunc     <- deparse(widgetui)
    uifunc[1]  <- paste0(info$name,"funcUI <- ",uifunc[1])
    srvfunc    <- deparse(widgetserver)
    srvcall    <- sub("function",paste0(info$name,"funcSRV"),srvfunc[1])
    srvfunc[1] <- paste0(info$name,"funcSRV <- ",srvfunc[1])
    return(list(tabs,items,uifunc,srvfunc,srvcall))
  })

  # Reading the init if applicable
  if(!is.null(init)) {initsc <- paste(c("  ",readLines(init),"\n "),collapse="\n  ")}else{initsc <- ""}

  # write everything to file and start app if applicable
  uilst <- list(widget_tabs = do.call(paste,c(lapply(allitems,"[[",1),sep=",\n      ")),
                widget_ui   = do.call(paste,c(lapply(allitems,"[[",2),sep=",\n      ")))
  uif   <- readLines(system.file("dashboard/ui.tmpl",package="shinyMixR"))
  uif   <- whisker::whisker.render(uif,data=c(uilst,setwd=paste0("setwd('",normalizePath(wd,winslash = "/"),"')")))
  srvf  <- readLines(system.file("dashboard/server.tmpl",package="shinyMixR"))
  srvf  <- whisker::whisker.render(srvf,data=list(server_logic=paste(initsc,do.call(paste,c(lapply(allitems,"[[",5),sep="\n  ")))))

  if(!file.exists(outloc)) dir.create(outloc)
  cat(do.call(c,lapply(allitems,"[[",3)),file=paste0(outloc,"/ui_functions.r"),sep="\n")
  cat(do.call(c,lapply(allitems,"[[",4)),file=paste0(outloc,"/srv_functions.r"),sep="\n")
  cat(uif,file=paste0(outloc,"/ui.r"),sep="\n")
  cat(srvf,file=paste0(outloc,"/server.r"),sep="\n")
  dir.create(paste0(outloc,"/www"),showWarnings = FALSE)
  file.copy(system.file("dashboard/logonlmixr.png",package="shinyMixR"),paste0(outloc,"/www/logonlmixr.png"))
  file.copy(system.file("dashboard/logoshinyMixR.png",package="shinyMixR"),paste0(outloc,"/www/logoshinyMixR.png"))

  if(runapp){
    # Decided not to provide ability to run in separate R session (although can be done with callr). The main reason is that it
    # is difficult/impossible to close off both the app and the underlying R session
    shiny::runApp(outloc,...)
  }
}
