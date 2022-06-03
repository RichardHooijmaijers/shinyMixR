# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="fit",
             title="Fit plots",
             icon="chart-line")

widgetui <- function() {
  tagList(
    selectInput("fit_fitLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE),
    actionButton("fit_make", "Create plot",icon=icon("play")),
    actionButton("fit_save", "Save plot",icon=icon("save")),
    br(),br(),
    plotOutput("fit_plot",width="80%")
  )
}
widgetserver <- function(input,output,session){
  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="fit"){
      updateSelectInput(session, "fit_fitLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$fit_fitLst)
    }
  },ignoreInit=TRUE)

  # Create fit plot (type of plot taken from settings!)
  fitpl <- function(inp,saveit=FALSE){
    res <- readRDS(paste0("shinyMixR/",inp$fit_fitLst[1],".res.rds"))
    if(!saveit){
      fit_plot(res,type=input$settings_plott)
    }else{
      savnm  <- ifelse(inp$fit_typeout=="PDF",paste0(inp$fit_savename,".tex"),paste0(inp$fit_savename,".html"))
      fit_plot(res,type=input$settings_plott,mdlnm=inp$fit_fitLst,outnm=savnm,show=inp$fit_showres)
      removeModal()
    }
  }
  fitplm <- eventReactive(input$fit_make,fitpl(input))
  output$fit_plot   = renderPlot(fitplm())

  # Save results
  fitsave = modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
                        textInput("fit_savename","Save as",value="fits"),
                        radioButtons("fit_typeout", "Save type", choices = c("HTML","PDF"), inline = TRUE),
                        checkboxInput("fit_showres","Show on save",value=FALSE),
                        actionButton("fit_save2", "Save",icon=icon("save")),br(),
                        HTML("Modal will close when output is saved"),
                        conditionalPanel(condition="input.fit_typeout =='PDF'",
                          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
                        ))
  observeEvent(input$fit_save,showModal(fitsave))
  observeEvent(input$fit_save2, fitpl(input,saveit=TRUE))

}
