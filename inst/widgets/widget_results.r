# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="results",
             title="Analysis results",
             icon="book")

widgetui <- function() {
  tagList(
    fluidRow(
      column(3,
        actionButton("results_refresh", "Refresh",icon=icon("refresh")),br(),br(),
        actionButton("results_showit", "Show results",icon=icon("book")),br(),
        radioButtons("results_type", "", choices = c("HTML","PDF"), inline = TRUE),br(),
        textInput("results_name","Name results",value="Report")
      ),
      column(4,selectInput("results_modLst","Model(s)",list.dirs("analysis",recursive=FALSE,full.names=FALSE),multiple=FALSE,size=10,selectize=FALSE)),
      column(4,selectInput("results_resLst","Result(s)","",multiple=TRUE,size=10,selectize=FALSE))
    ),br(),
    conditionalPanel(condition="input.results_type =='PDF'",
      HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
    )
  )
}
widgetserver <- function(input,output,session){

  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="results"){
      updateSelectInput(session, "results_modLst", choices = list.dirs("analysis",recursive=FALSE,full.names=FALSE))
    }
  },ignoreInit=TRUE)

  # Refresh files and report
  reschfunc <- function(){
    if(input$results_modLst!="" && !is.null(input$results_modLst))  updateSelectInput(session,"results_resLst",choices=list.files(paste0("analysis/",input$results_modLst),pattern=ifelse(input$results_type=="PDF","\\.pdf$","\\.html$")))
  }
  observeEvent(input$results_refresh, updateSelectInput(session,"results_modLst",choices=list.dirs("analysis",recursive=FALSE,full.names=FALSE)),ignoreInit = TRUE)
  observeEvent(input$results_modLst,reschfunc(),ignoreInit = TRUE)
  observeEvent(input$results_type,reschfunc(),ignoreInit = TRUE)

  # Show results
  observeEvent(input$results_showit,{
    if(input$results_modLst!="" && !is.null(input$results_modLst) && !is.null(input$results_resLst)){
      if(input$results_type=="PDF"){
        if(length(input$results_resLst)==1){
          if(Sys.info()['sysname']=="Darwin"){
            try(system(paste0("open \"analysis/",input$results_modLst,"/",input$results_resLst,"\""),wait=FALSE))
          }else if(Sys.info()['sysname']=="Linux"){
            try(system(paste0("xdg-open 'analysis/",input$results_modLst,"/",input$results_resLst,"'")))
          }else if(Sys.info()['sysname']=="Windows"){
            try(shell(paste0("\"/analysis/",input$results_modLst,"/",input$results_resLst,"\""),wait=FALSE))
          }
        }else{
          ftr <- paste0("analysis/",input$results_modLst,"/",sub("\\.pdf$",".tex.rawtex",input$results_resLst))
          ftr <- ftr[file.exists(ftr)]
          R3port::ltx_combine(list(ftr),out=paste0("analysis/",input$results_modLst,"/",input$results_name,".tex"),show=TRUE)
        }
      }else if(input$results_type=="HTML"){
        if(length(input$results_resLst)==1){
          if(Sys.info()['sysname']=="Darwin"){
            try(system(paste0("open '",normalizePath(paste0("analysis/",input$results_modLst,"/",input$results_resLst)),"'"),wait=FALSE))
          }else{
            utils::browseURL(paste0("file://",normalizePath(paste0("analysis/",input$results_modLst,"/",input$results_resLst))))
          }
        }else{
          ftr <- paste0("analysis/",input$results_modLst,"/",sub("\\.html$",".html.rawhtml",input$results_resLst))
          ftr <- ftr[file.exists(ftr)]
          R3port::html_combine(list(ftr),out=paste0("analysis/",input$results_modLst,"/",input$results_name,".html"),show=TRUE,
                              template=paste0(system.file(package="R3port"),"/bootstrap.html"))
        }
      }
    }else{
      shinyWidgets::sendSweetAlert(session = session, title="Report",text = "Select folder and results for report" ,type = "error")
    }
  })
}
