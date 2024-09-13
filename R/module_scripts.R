#------------------------------------------ module_scripts_ui ------------------------------------------
#' Run script module for UI
#'
#' @description Shiny module for running scripts
#'
#' @param id Module id
#' 
#' @export
module_scripts_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("runscript"), label = "Run Script",icon=icon("code"))
}
#------------------------------------------ module_scripts_server ------------------------------------------
#' Run script module for server
#' 
#' @param id Module id
#' @param files character vector of files to apply the scripts on, usually a reactive
#' @param scripts character vector of scripts that can be applied on files, usually a reactive
#' @param loc character with the location where the temp scripts are saved (created when not existing)
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_scripts_server <- function(id, files=NULL, scripts=NULL, loc="temp", r) {
  # Decided that the files and script arguments should contain the path as well 
  # In the end the modal will show the basenames but on the background the entire 
  # name is used. The script will not set a working directory (it is the app dir by default)
  # and the script shows the complete names of the selected files. Currently the module is
  # set-up to use multiple script locations but a single location for the files
  moduleServer(id, function(input, output, session) {
    
    # Set reactive value for random uid for scripts
    uid <- reactiveVal("")
    # Create dataframe for scripts to enable different locations:
    dffun <- function(files,scripts){
      dff      <- data.frame(nam=scripts,num=as.numeric(factor(dirname(scripts),levels=unique(dirname(scripts)))),rn=1:length(scripts))
      dff$bn   <- basename(dff$nam)
      addl     <- dff[!duplicated(dff$num),]
      addl$nam <- addl$bn <- "---"
      addl$num <- addl$num + 0.1
      dff      <- rbind(dff,addl)
      dff      <- dff[order(dff$num,dff$rn),]
      dff      <- dff[-nrow(dff),]
      if(length(unique(dff$num))==1) dff$num <- ""
      dff$bn2  <- ifelse(dff$bn!="---" & dff$num!="",paste0(dff$bn," (",dff$num,")"),dff$bn)
      dff$loc  <- paste(normalizePath(dirname(dff$nam),winslash = "/"))
      dff$loc  <- ifelse(dff$num!="",paste0(dff$loc," (",dff$num,")"),dff$loc)
      
      df2      <- data.frame(nam=normalizePath(files,winslash = "/"),bn=basename(files))
      return(list(dff=dff,df2=df2))
    }
    # Function for the first modal
    scriptmodal1 <- function(dflist){
      ns <- session$ns
      modalDialog(title="Scripting",easyClose = TRUE,size="xl",fade=FALSE,
                  div(id="scrmodal1",div(id="scrmodal2",
                                         fluidRow(
                                           column(6,selectInput(ns("files"),"File(s)",dflist$df2$bn,multiple=TRUE,size=15,selectize=FALSE,width='100%')),
                                           column(6,selectInput(ns("scripts"),"Scripts",dflist$dff$bn2,multiple=FALSE,size=15,selectize=FALSE,width='100%'))
                                         ),
                                         actionButton(ns("runscriptA"), "Run script",icon=icon("play")),br(),br(),
                                         span(paste("scripts located in:",paste(unique(dflist$dff$loc[dflist$dff$nam!="---"]),collapse = ", ")),style="font-size: 0.75em;"),br(),
                                         span("Be aware that script is submitted in separate R session when 'Run script' is clicked. Box below shows progress and state 'Script done' when finished",style="font-size: 0.75em;"),br(),
                                         div(verbatimTextOutput(ns("scriptprogress")),class="card card-body bg-light p-0") # wrapped in div because well class is no longer present in bs4
                  ))
      )  
    }
    # Function to update the modal in case of arguments 
    # (make sure that arguments are wrapped in ns() in script, e.g. #inp# checkboxInput(ns("omitfixed"),...))
    scriptmodal2 <- function(allarg){
      ns <- session$ns
      modin <- try(tagList(lapply(allarg,function(x) eval(parse(text=x)))))
      if("try-error"%in%class(modin)) return('inputs not made correctly!')
      modinf <- tagList(div(id="scrmodal2",fluidRow(
        column(6,modin[[1]],actionButton(ns("runscriptB"), "Run script",icon=icon("play"))),
        column(6,div(verbatimTextOutput(ns("scriptprogress")),class="card card-body bg-light p-0")))))
      removeUI(paste0("#","scrmodal2"),session=session,immediate=TRUE)
      insertUI(paste0("#","scrmodal1"),ui=modinf,where="afterBegin",session=session,immediate=TRUE)  
    }
    # Function to run script using system calls
    runRscript <- function(id,script,allinputs){
      writeLines(paste("Run",allinputs$scripts,"for file(s)",paste(allinputs$files,collapse = ", ")),paste0(normalizePath(loc),"/scriptres",uid(),".out"))
      if(Sys.info()['sysname']=="Windows"){
        shell(paste0(R.home("bin"), "/Rscript \"", script,  "\" >> \"",normalizePath(loc),"/scriptres",id,".out\" 2>&1"),wait=FALSE)
      }else{
        system(paste0(R.home("bin"), "/Rscript \"", script,  "\" >> \"",normalizePath(loc),"/scriptres",id,".out\" 2>&1"),wait=FALSE)
      }
    }
    
    # This is the first observer to open up the modal
    observeEvent(input$runscript,{
      dflist <- dffun(files(),scripts())
      showModal(scriptmodal1(dflist))
    })
    # this is the second observer to run a script with arguments or to open modal in case of arguments
    observeEvent(input$runscriptA,{
      if(!isTruthy(input$scripts) || !isTruthy(input$files)){
        myalert("please select both script and model",type = "error")
      }else{
        uid(stringi::stri_rand_strings(1,6))
        dflist  <- dffun(files(),scripts())
        if(!file.exists(loc)) dir.create(loc,recursive=TRUE)
        allinp  <- reactiveValuesToList(input)
        scrcont <- readLines(dflist$dff$nam[dflist$dff$bn2==allinp$scripts])
        tmpsc   <- paste0(loc,"/",allinp$scripts,".",uid(),".r")
        if(any(grepl("^#inp#",scrcont))){
          # Actions in case script has arguments (replace content of current modal)
          warg  <- gsub("^#inp#","",scrcont[grepl("^#inp#",scrcont)])
          scriptmodal2(warg)
        }else{
          # Actions in case script does not has arguments
          writeLines(c(paste0("files <- c(", paste(shQuote(dflist$df2$nam[dflist$df2$bn==allinp$files]),collapse = ", "),")"),scrcont),tmpsc)
          runRscript(uid(),tmpsc,allinp)
          r$uids_running <- 1
        }
      }
    })
    
    # this is the third observer to run a script with given arguments
    observeEvent(input$runscriptB,{
      uid(stringi::stri_rand_strings(1,6))
      if(!file.exists(loc)) dir.create(loc,recursive=TRUE)
      allinp  <- reactiveValuesToList(input)
      dflist  <- dffun(files(),scripts())
      scrcont <- readLines(dflist$dff$nam[dflist$dff$bn2==allinp$scripts])
      tmpsc   <- paste0(loc,"/",allinp$scripts,".",uid(),".r")
      
      warg    <- gsub("^#inp#","",scrcont[grepl("^#inp#",scrcont)])
      # getParseData does not work here, method below assume that id's are wrapped in ns and id is first argument!
      # allid   <- getParseData(parse(text=warg), includeText = TRUE)
      # allid   <- allid[grepl("ns\\(.*\\)",allid$text) & allid$parent!=0,]
      # allid   <- gsub("^ns\\(|\\)|\\\"","",allid$text)
      allid  <- sapply(warg,function(x) as.character(as.list(str2lang(x))[[2]])[2])
      
      inplst <- allinp[names(allinp)%in%allid]
      inplst <- inplst[!sapply(inplst,is.null)]
      inplst <- lapply(inplst,function(x) if(typeof(x)=='character') shQuote(x) else x)
      inplst <- sapply(1:length(inplst), function(x) ifelse(length(inplst[[x]])>1,paste0(names(inplst)[[x]]," <- c(",paste(inplst[[x]],collapse=","),")"),paste(names(inplst)[[x]],inplst[[x]],sep=" <- ")))
      writeLines(c(paste0("files <- c(", paste(shQuote(dflist$df2$nam[dflist$df2$bn==allinp$files]),collapse = ", "),")"),inplst,scrcont),tmpsc)    
      runRscript(uid(),tmpsc,allinp)
      r$uids_running <- 1
    })
    
    monout <- reactive({
      
      req(file.exists(paste0(normalizePath(loc), "/scriptres", uid(), ".out")))
      
      txt <- paste(readLines(paste0(normalizePath(loc), "/scriptres", uid(), ".out")), collapse = "\n")
      
      if (r$uids_running > 0) {
        
        invalidateLater(1000, session)
        
        if (grepl("Script done", txt)) {
          #print("UID finished running")
          
          r$uids_running <- 0
        }
        
        return(txt)
        
      } else {
        return(txt)
      }
    })
    
    output$scriptprogress <- renderText(monout())
    
  })
}
