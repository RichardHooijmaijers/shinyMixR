# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="run",
             title="Run model(s)",
             icon="forward")

widgetui <- function() {
 tagList(
   selectInput("runmod_runLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,selectize = TRUE),
   actionButton("runmod_runMdl", "Run Model(s)",icon=icon("play")),
   #actionButton("runmod_showIt", "Show progress",icon=icon("spinner")),
   checkboxInput("runmod_addCWRES","Add CWRES to output",value=TRUE),
   checkboxInput("runmod_addNPDE","add NPDE to output",value=TRUE),
   br(),br(),
   verbatimTextOutput("runmod_progrTxt"),
   tags$head(tags$style("#runmod_progrTxt{overflow-y:scroll; max-height: 600px;}"))
  )
}
widgetserver <- function(input,output,session){
  # Run model
  observeEvent(input$runmod_runMdl,{
    unlink(list.files(paste0("shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE))
    #showNotification("model(s) submitted, wait for progress log to pop-up!")
    shinyWidgets::sendSweetAlert(session = session, title="Run",text = "model(s) submitted, wait for progress log to pop-up!" ,type = "success")
    if(!is.null(input$runmod_runLst)){
      lapply(input$runmod_runLst,function(mods) run_nmx(mods,proj_obj,addcwres=input$runmod_addCWRES,addnpde=input$runmod_addNPDE))
    }
  })
  # Monitor runs
  runmodmonit <- reactivePoll(500, session,
    checkFunc = function() {
      progf <- list.files("shinyMixR/temp",pattern="prog\\.txt$",full.names = TRUE)
      if (length(progf)>0)
        max(file.info(progf)$mtime)
      else
        ""
    },
    valueFunc = function() {
      progFn  <- list.files("shinyMixR/temp",pattern="prog\\.txt$",full.names = TRUE)
      paste(unlist(lapply(progFn,function(x) c(paste0("\n ***************",x,"***************"),readLines(x)))),collapse="\n")
    }
  )
  output$runmod_progrTxt <- renderText(runmodmonit())
}
