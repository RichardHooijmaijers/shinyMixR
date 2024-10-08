#------------------------------------------ module_reports_ui ------------------------------------------
#' Reporting module for UI
#'
#' @description Shiny module for reporting
#'
#' @param id Module id
#' 
#' @export
module_reports_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("createreport"), label = "Create report",icon=icon("book"))
}
#------------------------------------------ module_reports_server ------------------------------------------
#' Reporting module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_reports_server <- function(id, r) {
  moduleServer(id,function(input, output, session) {
    # Function for the modal
    reportmodal <- function(){
      ns <- session$ns
      modalDialog(title="Reports",easyClose = TRUE,size="l",fade=FALSE,
          fluidRow(
            column(6,selectInput(ns("models"),"Model(s)", list.dirs(paste0(r$this_wd,"/analysis"),recursive=FALSE,full.names=FALSE),
                   selected=NA,multiple=FALSE,size=15,selectize=FALSE,width='100%')),
            column(6,selectInput(ns("results"),"Result(s)","",multiple=TRUE,size=15,selectize=FALSE,width='100%'))
          ),
          actionButton(ns("showres"), "Show results",icon=icon("book")),br(),
          radioButtons(ns("type"), "", choices = c("HTML","PDF"), inline = TRUE),br(),
          textInput(ns("name"),"Name results",value="Report")
      )
    }  
    # This is the first observer to open up the modal
    observeEvent(input$createreport,{
      showModal(reportmodal())
    })

    # get files/reports in case model is selected
    reschfunc <- function(){
      if(input$models!="" && !is.null(input$models)){
        updateSelectInput(session,"results",choices=list.files(paste0(r$this_wd, "/analysis/",input$models),pattern=ifelse(input$type=="PDF","\\.pdf$","\\.html$")))
      }  
    }
    observeEvent(input$models,reschfunc(),ignoreInit = TRUE)
    observeEvent(input$type,reschfunc(),ignoreInit = TRUE)

    # Show results
    observeEvent(input$showres,{
      if(input$models!="" && !is.null(input$models) && !is.null(input$results)){
        if(input$type=="PDF"){
          if(length(input$results)==1){
            if(Sys.info()['sysname']=="Darwin"){
              try(system(paste0("open \"",r$this_wd,"/analysis/",input$models,"/",input$results,"\""),wait=FALSE))
            }else if(Sys.info()['sysname']=="Linux"){
              try(system(paste0("xdg-open '",r$this_wd,"/analysis/",input$models,"/",input$results,"'")))
            }else if(Sys.info()['sysname']=="Windows"){
              try(shell(paste0("\"",r$this_wd,"/analysis/",input$models,"/",input$results,"\""),wait=FALSE))
            }
          }else{
            ftr <- paste0(r$this_wd,"/analysis/",input$models,"/",sub("\\.pdf$",".tex.rawtex",input$results))
            ftr <- ftr[file.exists(ftr)]
            R3port::ltx_combine(list(ftr),out=paste0(r$this_wd,"/analysis/",input$models,"/",input$name,".tex"),show=TRUE)
          }
        }else if(input$type=="HTML"){
          if(length(input$results)==1){
            if(Sys.info()['sysname']=="Darwin"){
              try(system(paste0("open '",normalizePath(paste0(r$this_wd,"/analysis/",input$models,"/",input$results)),"'"),wait=FALSE))
            }else{
              utils::browseURL(paste0("file://",normalizePath(paste0(r$this_wd,"/analysis/",input$models,"/",input$results))))
            }
          }else{
            ftr <- paste0(r$this_wd,"/analysis/",input$models,"/",sub("\\.html$",".html.rawhtml",input$results))
            ftr <- ftr[file.exists(ftr)]
            R3port::html_combine(list(ftr),out=paste0(r$this_wd,"/analysis/",input$models,"/",input$name,".html"),show=TRUE,
                                template=paste0(system.file(package="R3port"),"/bootstrap.html"))
          }
        }
      }else{
        myalert("Select folder and results for report",type = "error")
      }
    })  
  })
}
