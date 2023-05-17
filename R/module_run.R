#------------------------------------------ module_run_ui ------------------------------------------
#' Run model module for UI
#'
#' @description Shiny module for running models
#'
#' @param id,input,output,session Internal parameters for {shiny}
#' 
#' @noRd
#' @export
module_run_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("runLst"),"Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,selectize = TRUE),
    actionButton(ns("runMdl"), "Run Model(s)",icon=icon("play")),
    actionButton(ns("monMdl"), "Monitor Model(s)",icon=icon("play")),br(),br(),
    checkboxGroupInput(ns("addExtra"),label=NULL,choices=c("Add CWRES to output","Add NPDE to output"),selected=c("Add CWRES to output","Add NPDE to output"),inline=TRUE),
    div(verbatimTextOutput(ns("progrTxt")),class="card card-body bg-light p-0",id="progph"),
   
  )
}
#------------------------------------------ module_run_server ------------------------------------------
#' Run model module for server
#' @param tabswitch reactive value that monitors the tabswitch
#' @noRd 
#' @export
module_run_server <- function(id,tabswitch) {
  moduleServer(id, function(input, output, session) {
    #print("Im here")
    # Adapt/update model list 
    observeEvent(tabswitch(),{
      if(tabswitch()=="run"){
        updateSelectInput(session, "runLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$runmod_runLst)
      }
    },ignoreInit=TRUE)

    # Run model
    observeEvent(input$runMdl,{
      unlink(list.files(paste0("shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE))
      # Perform tests before running
      if(!is.null(input$runLst)){
        proj     <- get("proj_obj",pos = .GlobalEnv)
        checkall <- unlist(sapply(input$runLst,function(x){
          chk    <- proj[[x]]$model
          chksrc <- try(source(chk,local=TRUE),silent=TRUE)
          if("try-error"%in%class(chksrc)){
            return("syntax error within model file")
          }else if(!tools::file_path_sans_ext(basename(chk))%in%ls()){
            return("model and function name do not comply")
          }
        }))
        if(length(checkall)>0){
          myalert(paste("The following issues occured:",paste0(names(checkall),": ",checkall,collapse=", ")),type = "error")
        }else{
          myalert("model(s) submitted, wait for progress log to pop-up!",type = "succes")
          addcwres <- ifelse("Add CWRES to output"%in%input$addExtra,TRUE,FALSE)
          addnpde  <- ifelse("Add NPDE to output"%in%input$addExtra,TRUE,FALSE)
          lapply(input$runLst,function(mods) run_nmx(mods,proj_obj,addcwres=addcwres,addnpde=addnpde))
        }
      }else{
        myalert("Please select models to run",type = "error")
      }
    })
    # Get progress log
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
        paste(unlist(lapply(progFn,function(x) c(paste0("\n ***************",x,"***************"),readLines(x, warn = FALSE)))),collapse="\n")
      }
    )
    output$progrTxt <- renderText(runmodmonit())
    # Monitor all external runs
    rv <- reactiveValues(montbl=NULL)
    monmodal <- function(){
      ns <- session$ns
      modalDialog(title="All running models",easyClose = TRUE,size="l",DT::DTOutput(ns("mon_out")),actionButton(ns("killMdl"), "Kill Model",icon=icon("play")))
    }
    hr_out <- eventReactive(input$monMdl, {
      ret <- ps::ps()
      nfo <- lapply(ret$ps_handle, function(x){
        runn <- ps::ps_is_running(x)
        ret  <- data.frame(modf=character(1),time=numeric(1),pid=numeric(1))
        if(runn){
          cmd  <- try(ps::ps_cmdline(x),silent=TRUE)
          if(any(grepl("shinyMixR/temp/script",cmd)) & !any(grepl("prog\\.txt",cmd))){
            scrf   <- sub("--file=","",cmd[grepl("shinyMixR/temp/script",cmd)])
            scrf   <- try(readLines(scrf),silent=TRUE)
            runin  <- grepl("modres <- try\\(nlmixr\\(",scrf) 
            if(any(runin)){
              modf     <- gsub("modres <- try\\(nlmixr\\(|,.*","",scrf[runin])
              ret$modf <- modf
              ret$time <- ps::ps_cpu_times(x)["user"]
              ret$pid  <- ps::ps_pid(x)
            }
          }
        }
        return(ret)
      })
      nfo <- data.frame(do.call(rbind,nfo))
      nfo <- nfo[nfo$modf!="",]
      rv$montbl <- nfo
      nfo
    })
    output$mon_out <- DT::renderDT(hr_out())
    observeEvent(input$monMdl,{showModal(monmodal())},ignoreInit = TRUE)
    observeEvent(input$killMdl,{
      pid <- rv$montbl[input$mon_out_rows_selected,] 
      try(tools::pskill(pid))
      removeModal()
    },ignoreInit = TRUE)

  })
}
