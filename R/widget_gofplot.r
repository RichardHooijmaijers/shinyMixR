# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ gofUI ------------------------------------------
#' @export
gofUI <- function() {
  tagList(
    selectInput("gofLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE),
    actionButton("gofMdl", "Create GOF",icon=icon("play")),
    actionButton("saveGOF", "Save plot",icon=icon("save")),
    shinyBS::bsModal("modalSaveGOF","Save results","saveGOF",
      textInput("nameGOF","Save as",value="GOF"),
      radioButtons("typeGOF", "Save type", choices = c("HTML","PDF"), inline = TRUE),
      checkboxInput("showGOF","Show on save",value=FALSE),
      actionButton("saveGOF2", "Save",icon=icon("save")),br(),
      HTML("Modal will close when output is saved"),
      conditionalPanel(condition="input.typeGOF =='PDF'",
        HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
      )
    ),
    br(),br(),
    plotOutput("gofPlt",width="80%")
  )
}
#------------------------------------------ gofPlot ------------------------------------------
#' @export
# Function to create and save a gof plots
# save: logical indicating if the resulting plot should be saved
gofPlot <- function(inp,session,projloc=".",saveit=FALSE){
  res <- readRDS(paste0(projloc,"/shinyMixR/",inp$gofLst[1],".res.rds"))
  if(!saveit){
    gof_plot(res,type=inp$plotType,projloc=projloc)
  }else{
    savnm  <- ifelse(inp$typeGOF=="PDF",paste0(inp$nameGOF,".tex"),paste0(inp$nameGOF,".html"))
    gof_plot(res,type=inp$plotType,mdlnm=inp$gofLst,outnm=savnm,projloc=projloc,show=inp$showGOF)
    shinyBS::toggleModal(session,"modalSaveGOF","close")
    updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projloc,"/analysis"),recursive=FALSE))
  }
}
