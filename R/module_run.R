#------------------------------------------ module_run_ui ------------------------------------------
#' Run model module for UI
#'
#' @description Shiny module for running models
#'
#' @param id Module id
#' @param proj_obj Project object
#' 
#' @export
module_run_ui <- function(id, proj_obj) {
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
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_run_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    #print("Im here")
    # Adapt/update model list 
    observeEvent(r$active_tab,{
      if(r$active_tab=="run"){
        updateSelectInput(session, "runLst", choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=input$runmod_runLst)
      }
    },ignoreInit=TRUE)

    # Run model
    observeEvent(input$runMdl,{
      unlink(list.files(paste0(r$this_wd,"/shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE))
      # Perform tests before running
      if(!is.null(input$runLst)){
        proj     <- r$proj_obj
        checkall <- unlist(sapply(input$runLst,function(x){
          chk    <- proj[[x]]$model
          chkdat <- proj[[x]]$modeleval$meta$data 
          chksrc <- try(source(chk,local=TRUE),silent=TRUE)
          if("try-error"%in%class(chksrc)){
            return("syntax error within model file")
          }else if(!tools::file_path_sans_ext(basename(chk))%in%ls()){
            return("model and function name do not comply")
          }else if(!proj[[x]]$modeleval$meta$est%in%nlmixr2est::nlmixr2AllEst()){
            return("specified estimation method not available")
          }else if(!any(paste0(chkdat,c(".csv",".rds"))%in%list.files(paste0(r$this_wd,"/data"))) && !exists(chkdat)){
            return("specified dataset could not be found")
          }
        }))
        if(length(checkall)>0){
          myalert(paste("The following issues occured:",paste0(names(checkall),": ",checkall,collapse=", ")),type = "error")
        }else{
          myalert("model(s) submitted, wait for progress log to pop-up!",type = "succes")
          addcwres <- ifelse("Add CWRES to output"%in%input$addExtra,TRUE,FALSE)
          addnpde  <- ifelse("Add NPDE to output"%in%input$addExtra,TRUE,FALSE)
          lapply(input$runLst,function(mods) run_nmx(mods, r$proj_obj, addcwres=addcwres,addnpde=addnpde,projloc=r$this_wd))
          # check for any existing finished models, and remove from r$finished_models if input$runList is in them
          if (length(r$finished_models) > 0) {
            r$finished_models <- r$finished_models[-which(grepl(paste0(input$runLst, collapse = "|"), r$finished_models))]
          } else {
            r$finished_models <- character(0)
          }
          r$models_running <- r$models_running + length(input$runLst)
        }
      }else{
        myalert("Please select models to run",type = "error")
      }
    })
    
    runmodmonit <- reactive({
      
      progFn  <- list.files(paste0(r$this_wd,"/shinyMixR/temp"),pattern="prog\\.txt$",full.names = TRUE)
      txt <- lapply(progFn,function(x) c(paste0("\n ***************",x,"***************"),readLines(x, warn = FALSE)))
      
      if (r$models_running > 0) {
        
        invalidateLater(1000, session)
        
        if (any(grepl("run finished", txt))) {

          finished_models <- progFn[which(grepl("run finished", txt))]
          
          if (!all(finished_models %in% r$finished_models)) {
            
            print(sprintf("%s model(s) finished running", length(finished_models)))
            
            r$finished_models <- c(isolate(r$finished_models), finished_models)
            r$models_running <- length(setdiff(progFn, finished_models))
            r$model_updated <- isolate(r$model_updated) + 1
            
            exportTestValues(
              model_updated = r$model_updated
            )
          }
        }
        
        return(paste(unlist(txt), collapse = "\n"))
        
      } else {
        return(paste(unlist(txt), collapse = "\n"))
      }
    })
    
    output$progrTxt <- renderText(runmodmonit())
    
    model_updated_d <- debounce(reactive(r$model_updated), millis = 1000)
    
    observe({
      req(model_updated_d() > 0)
      r$proj_obj <- get_proj(r$this_wd)
    })
    
    # Disable suspend for output$myplot, otherwise necessary reactives
    # don't trigger when user is not on this tab anymore
    outputOptions(output, "progrTxt", suspendWhenHidden = FALSE)
    
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
