# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ oviewUI ------------------------------------------
#' @export
overviewUI <- function(projlst) {
  tagList(
    shinyFiles::shinyDirButton('projloc', 'Project selection', 'Please select a folder'),
    actionButton("refrOview", "Refresh",icon=icon("refresh")),
    actionButton("adptOview", "Adapt model notes",icon=icon("list")),
    actionButton("delOview", "Delete model(s)",icon=icon("remove")),
    br(),br(),
    shinyBS::bsModal("modalAdptOview","Adapt model notes","",
      selectInput("modelAdpt","Model",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
      sliderInput("impAdpt", "Importance", 0, 5, 0, step = 1, round = TRUE),
      textInput("descAdpt","Description",value=""),
      selectInput("refAdpt","Reference",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
      textInput("dataAdpt","Data",value=""),
      selectInput("estAdpt","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
      actionButton("adptOview2", "Go",icon=icon("play"))
    ),
    shinyBS::bsModal("modalDelOview","Delete model(s)","",
      checkboxInput("delAllMod","Delete all models and results",value=TRUE),
      actionButton("delOview2", "Go",icon=icon("play"))
    ),
    shinydashboard::box(DT::dataTableOutput('oviewTable'),width=NULL,title = span(id="projTitle1",span(id="projTitle2","Overview")), solidHeader = TRUE, status = "primary",collapsible = TRUE),
    shinydashboard::box(
      actionButton("createTree", "make tree",icon=icon("tree")),
      collapsibleTree::collapsibleTreeOutput("treeOut"),
      width=NULL,title = "Tree View",solidHeader = TRUE, status = "primary",collapsible = TRUE,collapsed = TRUE
    )
   )
}
#------------------------------------------ adaptOverview ------------------------------------------
#' @export
# Adapt the model overview with the meta data for the selected model
# type: character. In case set to "modal" it will take the model name from the overview otherwise from the input element
adaptOverview <- function(projlst,inp,session,type="modal"){
  if(length(projlst)==0) return()
  msel <- ifelse(type=="modal",names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected],inp$modelAdpt)
  # if(length(msel)!=0 && msel!="" && !is.na(msel)) eval(parse(text=body(eval(parse(text=readLines(projlst[[msel]]$model))))[4]))
  # Only works if get_proj has geteval=TRUE which is the default for the shinyMixR interface
  meta <- projlst[[msel]]$modeleval$meta
  #print(meta)
  updateSelectInput(session,"refAdpt",choices=c("",names(projlst)[names(projlst)!="meta"]))
  updateMdlchar(session,meta,"Adpt")
  #print(names(projlst)[names(projlst)!="meta"])
  updateSelectInput(session,"modelAdpt",selected=msel,choices=names(projlst)[names(projlst)!="meta"])
  if(type=="modal") shinyBS::toggleModal(session,"modalAdptOview","open")
}
#------------------------------------------ AdaptModel ------------------------------------------
#' @export
# Adapt and write the model based on the new meta data changed by the user
adaptModel <- function(projlst,inp,session){
  if(inp$modelAdpt!=""){
    metanfo <- reactiveValuesToList(inp)[c("impAdpt","descAdpt","refAdpt","dataAdpt","estAdpt")]
    names(metanfo) <- sub("Adpt","",names(metanfo))
    # Had to place output of adpt_meta in object otherwise writeLines did not work
    # Added assign projlst so latest changes are saved and are used when multiple changes are made
    towr <- adpt_meta(paste0(projwd,"/models/",inp$modelAdpt,".r"),metanfo)
    writeLines(towr,paste0(projwd,"/models/",inp$modelAdpt,".r"))
    assign(deparse(substitute(projlst)),get_proj(projwd),pos = .GlobalEnv,inherits=TRUE)
    shinyBS::toggleModal(session,"modalAdptOview","close")
    proxy = DT::dataTableProxy('oviewTable')
    # Could not use ColReorder extension in combination with replaceData!
    DT::replaceData(proxy, overview(projloc=projwd), rownames = FALSE)
  }
}
#------------------------------------------ delOverview ------------------------------------------
#' @export
# delete the the selected model(s)
delOverview <- function(projlst,inp,session){
  msel <- names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected]
  if(inp$delAllMod) {
    try(file.remove(paste0(projwd,"/shinyMixR/",msel,".res.rds")))
    try(file.remove(paste0(projwd,"/shinyMixR/",msel,".ressum.rds")))
    try(unlink(paste0(projwd,"/analysis/",msel),recursive = TRUE))
  }
  try(file.remove(paste0(projwd,"/models/",msel,".r")))
  assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
  modnm <- names(get(deparse(substitute(projlst)),pos = .GlobalEnv))
  updateSelectInput(session,"editLst",choices=c("",modnm[modnm!="meta"]))
  for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=modnm[modnm!="meta"])
  shinyBS::toggleModal(session,"modalDelOview","close")
}
#------------------------------------------ selectProjFolder ------------------------------------------
#' @export
# select different project folder
selectProjFolder <- function(projlst,inp,session){

  projwd <- ifelse(length(shinyFiles::parseDirPath(c("Root"=.cwd), inp$projloc))==0,".",shinyFiles::parseDirPath(c("Root"=.cwd), inp$projloc))
  projwd <- normalizePath(projwd,winslash="/")
  if(inp$createsmf) create_proj(projwd) 
  removeUI(selector = "#projTitle2")
  insertUI(selector = "#projTitle1", ui=HTML(paste("<span id='projTitle2'>Overview - ",sub(normalizePath(.cwd,winslash="/"),".",projwd),"</span>")))
  assign("projwd",projwd,pos = .GlobalEnv)
  assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
  updateSelectInput(session,"editLst",choices=c("",names(projlst)[names(proj_obj)!="meta"]))
  for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=names(projlst)[names(projlst)!="meta"])
  updateSelectInput(session,"scriptFilLst",choices=list.files(paste0(projwd,"/scripts")))
  updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projwd,"/analysis"),recursive=FALSE,full.names=FALSE))
  if(file.exists(paste0(projwd,"/shinyMixR"))){
    oview <- overview(projloc=projwd)
    proxy = DT::dataTableProxy('oviewTable')
    DT::replaceData(proxy, oview, rownames = FALSE)
  }
}
#------------------------------------------ refreshOverview ------------------------------------------
#' @export
# refresh overview
refreshOverview <- function(){
  if(file.exists(paste0(projwd,"/shinyMixR"))){
    oview <- overview(projloc=projwd)
    proxy = DT::dataTableProxy('oviewTable')
    DT::replaceData(proxy, oview, rownames = FALSE)
  }
}
