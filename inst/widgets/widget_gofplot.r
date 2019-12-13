# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="gof",
             title="Goodness of fit",
             icon="chart-line")

widgetui <- function() {
  tagList(
    selectInput("gof_gofLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE),
    actionButton("gof_make", "Create plot",icon=icon("play")),
    actionButton("gof_save", "Save plot",icon=icon("save")),
    br(),br(),
    plotOutput("gof_plot",width="80%")
  )
}
widgetserver <- function(input,output,session){
  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="gof"){
      updateSelectInput(session, "gof_gofLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$gof_gofLst)
    }
  },ignoreInit=TRUE)

  # Create GOF plot (type of plot taken from settings!)
  gofpl <- function(inp,saveit=FALSE){
    res <- readRDS(paste0("shinyMixR/",inp$gof_gofLst[1],".res.rds"))
    if(!saveit){
      gof_plot(res,type=input$settings_plott)
    }else{
      savnm  <- ifelse(inp$gof_typeout=="PDF",paste0(inp$gof_savename,".tex"),paste0(inp$gof_savename,".html"))
      gof_plot(res,type=input$settings_plott,mdlnm=inp$gof_gofLst,outnm=savnm,show=inp$gof_showres)
    }
  }
  gofplm <- eventReactive(input$gof_make,gofpl(input))
  output$gof_plot   = renderPlot(gofplm())

  # Save results
  gofsave = modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
                        textInput("gof_savename","Save as",value="GOF"),
                        radioButtons("gof_typeout", "Save type", choices = c("HTML","PDF"), inline = TRUE),
                        checkboxInput("gof_showres","Show on save",value=FALSE),
                        actionButton("gof_save2", "Save",icon=icon("save")),br(),
                        HTML("Modal will close when output is saved"),
                        conditionalPanel(condition="input.gof_typeout =='PDF'",
                          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
                        ))
  observeEvent(input$gof_save,showModal(gofsave))
  observeEvent(input$gof_save2, gofpl(input,saveit=TRUE))

}
