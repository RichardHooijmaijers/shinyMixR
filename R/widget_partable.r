# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ partableUI ------------------------------------------
#' @export
partableUI <- function() {
  tagList(
    actionButton("savePars", "Save parameter table",icon=icon("save")),br(),br(),
    shinyBS::bsModal("modalSavePars","Save results","savePars",
      textInput("namePars","Save as",value="ParTable"),
      radioButtons("typePars", "Save type", choices = c("PDF","HTML"), inline = TRUE),
      checkboxInput("showPars","Show on save",value=FALSE),
      actionButton("savePars2", "Save",icon=icon("save"))
    ),
    shinydashboard::box(selectInput("parEstLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE)),
    shinydashboard::box(DT::dataTableOutput('parEstTbl'))
  )
}
#------------------------------------------ parTable ------------------------------------------
#' @export
# Function to create parameter table for one or multiple result files
# saveit: logical indicating if the resulting table should be saved
parTable <- function(inp,session,saveit=FALSE){
   obj     <- get_proj()
   if(!saveit){
     par_table(obj,models=inp$parEstLst)
   }else{
     savnm  <- ifelse(inp$typePars=="PDF",paste0(inp$namePars,".tex"),paste0(inp$namePars,".html"))
     par_table(obj,models=inp$parEstLst,outnm=savnm,show=inp$showPars)
     shinyBS::toggleModal(session,"modalSavePars","close")
     updateSelectInput(session,"resModLst",choices=list.dirs("./analysis",recursive=FALSE))
   }
}
