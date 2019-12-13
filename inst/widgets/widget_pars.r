# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="par",
             title="Parameter estimates",
             icon="table")

widgetui <- function() {
 tagList(
   actionButton("par_savePars", "Save parameter table",icon=icon("save")),br(),br(),
   shinydashboard::box(selectInput("par_EstLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE)),
   shinydashboard::box(DT::dataTableOutput('par_EstTbl'))
  )
}
widgetserver <- function(input,output,session){
  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="par"){
      updateSelectInput(session, "par_EstLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$par_EstLst)
    }
  },ignoreInit=TRUE)

  # Create parameter table
  parTable <- function(inp,projloc=".",saveit=FALSE){
     obj     <- get_proj(projloc=projloc)
     if(!saveit){
       par_table(obj,models=inp$par_EstLst)
     }else{
       savnm  <- ifelse(inp$par_typePars=="PDF",paste0(inp$par_namePars,".tex"),paste0(inp$par_namePars,".html"))
       par_table(obj,models=inp$par_EstLst,outnm=savnm,show=inp$par_showPars,projloc=projloc)
     }
  }
  output$par_EstTbl = DT::renderDataTable(parTable(input),rownames=FALSE,options=list(paging=FALSE,searching=FALSE))
  proxy2 = DT::dataTableProxy('par_EstTbl')
  observeEvent(input$par_EstLst, DT::replaceData(proxy2, parTable(input), rownames = FALSE))

  # Save results
  parsave = modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
                        textInput("par_namePars","Save as",value="ParTable"),
                        radioButtons("par_typePars", "Save type", choices = c("HTML","PDF"), inline = TRUE),
                        checkboxInput("par_showPars","Show on save",value=FALSE),
                        actionButton("par_savePars2", "Save",icon=icon("save")),br(),
                        HTML("Modal will close when output is saved"),
                        conditionalPanel(condition="input.par_typePars =='PDF'",
                          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
                        ))
  observeEvent(input$par_savePars,showModal(parsave))
  observeEvent(input$par_savePars2, parTable(input,session,saveit=TRUE))

}
