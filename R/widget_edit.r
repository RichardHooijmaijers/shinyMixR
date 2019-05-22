# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ editUI ------------------------------------------
#' @export
editUI <- function(projlst) {
  tagList(
    selectInput("editLst","Model(s)",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
    shinyAce::aceEditor("editor",value="",mode="r",theme = "solarized_light",height="600px",fontSize = 14),
    actionButton("saveMdl", "Save Model",icon=icon("save")),
    actionButton("saveMdlAs", "Save Model as",icon=icon("save")),
    shinyBS::bsModal("modalsaveAs","Save as","",size="small",
      textInput("nameSaveasmod","Name model",value=""),
      actionButton("saveMdlAs2", "Save",icon=icon("save"))
    ),
    actionButton("duplMdl", "Duplicate Model",icon=icon("copy")),
    shinyBS::bsModal("modalDupl","Duplicate model","",
      textInput("nameDupmod","Name new model",value=""),
      sliderInput("impDupmod", "Importance", 0, 5, 0, step = 1, round = TRUE),
      textInput("descDupmod","Description",value=""),
      selectInput("refDupmod","Reference",c("",names(projlst)[names(projlst)!="meta"]),multiple=FALSE,selectize = TRUE),
      textInput("dataDupmod","Data",value=""),
      selectInput("estDupmod","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
      actionButton("duplMdl2", "Go",icon=icon("play"))
    ),
    actionButton("newMdl", "New Model",icon=icon("file-text-o")),
    shinyBS::bsModal("modalnewMdl","New model","",
      textInput("nameNewmod","Name new model",value=""),
      selectInput("templNewmod","template",c("pk.1cmt.closed","pk.1cmt.des")),
      actionButton("newMdl2", "Go",icon=icon("play"))
    ),
    br(),br(),shinyBS::bsAlert("alertEdit")
)
}
#------------------------------------------ updateRunInputs ------------------------------------------
#' @export
# Function to update all the ui elements (when new model is created). Used in both duplModelSave() and newModelSave()
updateRunInputs <- function(projlst,session){
  # model names should be taken from object in global environment!
  modnm <- names(get(projlst,pos = .GlobalEnv))
  # Update all selection boxes when new model is made
  updateSelectInput(session,"editLst",choices=c("",modnm[modnm!="meta"]))
  for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=modnm[modnm!="meta"])
  shinyBS::createAlert(session,"alertEdit",content="Model saved",append=FALSE,alertId="alertEditID",style="success")
}
#------------------------------------------ udpateEditList ------------------------------------------
#' @export
# Function to update the editor when a model is selected
updateEditList <- function(projlst,inp,session){
  if(!is.null(projlst[[inp$editLst]]$model)){
    shinyBS::closeAlert(session,"alertEditID")
    shinyAce::updateAceEditor(session,"editor",value=paste(readLines(projlst[[inp$editLst]]$model),collapse="\n"))
  }
}
#------------------------------------------ saveModel ------------------------------------------
#' @export
# Write the content of the editor to text file on disk
saveModel <- function(projlst,inp,session){
  if(inp$editLst!=""){
    writeLines(inp$editor,projlst[[inp$editLst]]$model)
    assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
    # shinyBS seems to have a (minor) bug. Alerts are not closed, should be done manual in cases
    shinyBS::closeAlert(session, "alertEdit")
    shinyBS::createAlert(session,"alertEdit",content="Model saved",append=FALSE,alertId="alertEditID",style="success")
  }
}
#------------------------------------------ saveModelAsM ------------------------------------------
#' @export
# Open the save as modal
saveModelAsM <- function(session) shinyBS::toggleModal(session,"modalsaveAs","open")
#------------------------------------------ saveModelAs ------------------------------------------
#' @export
# Save the new model with a new name
saveModelAs <- function(projlst,inp,session){
  if(!grepl("run[[:digit:]]*\\.[r|R]",inp$nameSaveasmod)){
    shinyBS::toggleModal(session,"modalsaveAs","close")
    shinyBS::closeAlert(session, "alertEdit")
    shinyBS::createAlert(session,"alertEdit",content="naming of models should be 'run[digit(s)].r' (all lower case)",append=FALSE,alertId="alertEditID",style="danger")
  }else{
    # (if necessary) change function name to savename
    towr <- sub(inp$editLst,sub("\\.[r|R]","",inp$nameSaveasmod),inp$editor)
    writeLines(towr,paste0(projwd,"/models/",inp$nameSaveasmod))
    shinyBS::toggleModal(session,"modalsaveAs","close")
    assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
    updateRunInputs(deparse(substitute(projlst)),session)
    shinyAce::updateAceEditor(session,"editor",value=paste(readLines(projlst[[sub("\\.[r|R]","",inp$nameSaveasmod)]]$model),collapse="\n"))
    updateSelectInput(session,"editLst",selected=sub("\\.[r|R]","",inp$nameSaveasmod))
  }
}
#------------------------------------------ duplModelModal ------------------------------------------
#' @export
# Correctly open the modal in case duplicate model is selected
duplModelModal <- function(projlst,inp,session){
  if(inp$editLst!=""){
    updateTextInput(session,"nameDupmod",value=incr_mdl(paste0(inp$editLst,".r"),paste0(projwd,"/models")))
    if(length(inp$editLst)!=0) meta <- try(projlst[[inp$editLst]]$modeleval$meta)
    if(class(meta)=="try-error"){
      shinyBS::closeAlert(session, "alertEdit")
      shinyBS::createAlert(session,"alertEdit",content="Error in base model cannot be duplicated",append=FALSE,alertId="alertEditID",style="danger")
    }else{
      updateMdlchar(session,meta,"Dupmod")
      shinyBS::toggleModal(session,"modalDupl","open")
    }
  }
}
#------------------------------------------ DuplModelSave ------------------------------------------
#' @export
# Actually save the model to disk for duplicating model
duplModelSave <- function(projlst,inp,session){
  metanfo <- reactiveValuesToList(inp)[c("impDupmod","descDupmod","refDupmod","dataDupmod","estDupmod")]
  names(metanfo) <- sub("Dupmod","",names(metanfo))
  # Had to place output of adpt_meta in object otherwise writeLines did not work
  towr <- adpt_meta(paste0(projwd,"/models/",inp$editLst,".r"),metanfo)
  towr <- sub(inp$editLst,sub("\\.[r|R]","",inp$nameDupmod),towr)
  writeLines(towr,paste0(projwd,"/models/",inp$nameDupmod))
  shinyBS::toggleModal(session,"modalDupl","close")
  assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
  updateRunInputs(deparse(substitute(projlst)),session)
  shinyAce::updateAceEditor(session,"editor",value=paste(readLines(projlst[[sub("\\.[r|R]","",inp$nameDupmod)]]$model),collapse="\n"))
  updateSelectInput(session,"editLst",selected=sub("\\.[r|R]","",inp$nameDupmod))
}
#------------------------------------------ newModelModal ------------------------------------------
#' @export
# Correctly open the modal in case new model is selected
newModelModal <- function(inp,session){
  toincr <- ifelse(inp$editLst=="","run1.r",paste0(inp$editLst,".r"))
  updateTextInput(session,"nameNewmod",value=incr_mdl(toincr,paste0(projwd,"/models")))
  shinyBS::toggleModal(session,"modalnewMdl","open")
}
#------------------------------------------ NewModelSave ------------------------------------------
#' @export
# Actually save the model to disk for new model
newModelSave <- function(projlst,inp,session){
  mdl <- try(readLines(system.file(paste0("Other/",inp$templNewmod,".r"),package="shinyMixR")))
  if(class(mdl)!="try-error"){
    mdl <- sub("run1",sub("\\.[r|R]","",inp$nameNewmod),mdl)
    writeLines(mdl,paste0(projwd,"/models/",inp$nameNewmod))
    shinyBS::toggleModal(session,"modalnewMdl","close")
    assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
    updateRunInputs(deparse(substitute(projlst)),session)
    shinyAce::updateAceEditor(session,"editor",value=paste(readLines(projlst[[sub("\\.[r|R]","",inp$nameNewmod)]]$model),collapse="\n"))
    updateSelectInput(session,"editLst",selected=sub("\\.[r|R]","",inp$nameNewmod))
  }
}
