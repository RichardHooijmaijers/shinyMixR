# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ runUI ------------------------------------------
#' @export
runUI <- function() {
  tagList(
    selectInput("runLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,selectize = TRUE),
    actionButton("runMdl", "Run Model(s)",icon=icon("play")),
    actionButton("showIt", "Show progress",icon=icon("spinner")),
    checkboxInput("addCWRES","Add CWRES to output",value=TRUE),
    checkboxInput("addNPDE","add NPDE to output",value=TRUE),
    br(),br(),
    shinyBS::bsAlert("alertRunmdl"),br(),br(),
    verbatimTextOutput("progrTxt"),
    tags$head(tags$style("#progrTxt{overflow-y:scroll; max-height: 600px;}"))
  )
}
#------------------------------------------ runMod ------------------------------------------
#' @export
# Function to run one or multiple models async (to avoid freezing of app)
# templog: location where the temporary log files or progress files are stored
runMod <- function(projlst,inp,session,projloc="."){
  shinyBS::closeAlert(session,"alertRunmdlID")
  unlink(list.files(paste0(projloc,"/shinyMixR/temp"),pattern="run.*prog\\.txt$",full.names = TRUE))
  if(!is.null(inp$runLst)) lapply(inp$runLst,function(mods) run_nmx(mods,projlst,projloc=projloc,addcwres=inp$addCWRES,addnpde=inp$addNPDE))
  shinyBS::createAlert(session,"alertRunmdl",content="model(s) submitted. Click 'Show progress' to see current status",append=FALSE,alertId="alertRunmdlID",style="success")
}
#------------------------------------------ modProgr ------------------------------------------
#' @export
# Function to read in and show the temporary log file for one or multiple runs
# templog: location where the temporary log files or progress files are stored
modProgr <- function(projloc="."){
  progFn   <- list.files(paste0(projloc,"/shinyMixR/temp"),pattern="prog\\.txt$",full.names = TRUE)
  progFnr  <- unlist(lapply(progFn,function(x) c(paste0("\n ***************",x,"***************"),readLines(x))))
  cat(progFnr,sep="\n")
}
