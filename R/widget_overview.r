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
    actionButton("refrOview", "Refresh",icon=icon("refresh")),
    actionButton("adptOview", "Adapt model notes",icon=icon("list")),
    br(),br(),
    shinyBS::bsModal("modalAdptOview","Adapt model notes","",
      selectInput("modelAdpt","Model",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
      sliderInput("impAdpt", "Importance", 0, 5, 0, step = 1, round = TRUE),
      textInput("descAdpt","Description",value=""),
      selectInput("refAdpt","Reference",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
      textInput("dataAdpt","Data",value=""),
      selectInput("methodAdpt","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
      actionButton("adptOview2", "Go",icon=icon("play"))
    ),
    shinydashboard::box(DT::dataTableOutput('oviewTable'),width=NULL,title = "Overview", solidHeader = TRUE, status = "primary",collapsible = TRUE),
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
  msel <- ifelse(type=="modal",names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected],inp$modelAdpt)
  # if(length(msel)!=0 && msel!="" && !is.na(msel)) eval(parse(text=body(eval(parse(text=readLines(projlst[[msel]]$model))))[4]))
  # Only works if get_proj has geteval=TRUE which is the default for the shinyMixR interface
  meta <- projlst[[msel]]$modeleval$meta
  updateMdlchar(session,meta,"Adpt")
  updateSelectInput(session,"modelAdpt",selected=msel)
  if(type=="modal") shinyBS::toggleModal(session,"modalAdptOview","open")
}
#------------------------------------------ AdaptModel ------------------------------------------
#' @export
# Adapt and write the model based on the new meta data changed by the user
adaptModel <- function(projlst,inp,session){
  if(inp$modelAdpt!=""){
    metanfo <- reactiveValuesToList(inp)[c("impAdpt","descAdpt","refAdpt","dataAdpt","methodAdpt")]
    names(metanfo) <- sub("Adpt","",names(metanfo))
    # Had to place output of adpt_meta in object otherwise writeLines did not work
    towr <- adpt_meta(paste0("./models/",inp$modelAdpt,".r"),metanfo)
    writeLines(towr,paste0("./models/",inp$modelAdpt,".r"))
    shinyBS::toggleModal(session,"modalAdptOview","close")
    proxy = DT::dataTableProxy('oviewTable')
    DT::replaceData(proxy, overview(), rownames = FALSE)
  }
}
