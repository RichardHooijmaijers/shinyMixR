# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ fitUI ------------------------------------------
#' @export
fitUI <- function() {
  tagList(
    selectInput("fitLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE),
    actionButton("fitMdl", "Create Fit plots",icon=icon("play")),
    actionButton("saveFit", "Save plot",icon=icon("save")),
    shinyBS::bsModal("modalSaveFit","Save results","saveFit",
      textInput("nameFit","Save as",value="IndFit"),
      radioButtons("typeFit", "Save type", choices = c("HTML","PDF"), inline = TRUE),
      checkboxInput("showFit","Show on save",value=FALSE),
      actionButton("saveFit2", "Save",icon=icon("save")),br(),
      HTML("Modal will close when output is saved"),
      conditionalPanel(condition="input.typeFit =='PDF'",
        HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
      )
    ),
    br(),br(),
    plotOutput("fitPlt",width="80%")
  )
}
#------------------------------------------ fitPlot ------------------------------------------
#' @export
# Function to create and save a fit plot
# save: logical indicating if the resulting plot should be saved
fitPlot <- function(inp,session,projloc=".",saveit=FALSE){
  res <- readRDS(paste0(projloc,"/shinyMixR/",inp$fitLst[1],".res.rds"))
  if(!saveit){
    fit_plot(res,type=inp$plotType,projloc=projloc)
  }else{
    savnm  <- ifelse(inp$typeFit=="PDF",paste0(inp$nameFit,".tex"),paste0(inp$nameFit,".html"))
    fit_plot(res,type=inp$plotType,mdlnm=inp$fitLst,outnm=savnm,projloc=projloc,show=inp$showFit)
    shinyBS::toggleModal(session,"modalSaveFit","close")
    updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projloc,"/analysis"),recursive=FALSE))
  }
}
