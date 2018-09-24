# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ resUI ------------------------------------------
#' @export
resUI <- function() {
  tagList(
    fluidRow(
      column(3,
        actionButton("refreshRes", "Refresh",icon=icon("refresh")),br(),br(),
        actionButton("showAllRes", "Show results",icon=icon("book")),br(),
        radioButtons("typeRes", "", choices = c("HTML","PDF"), inline = TRUE),br(),
        textInput("nameRes","Name results",value="Report")
      ),
      column(4,selectInput("resModLst","Model(s)","",multiple=FALSE,size=10,selectize=FALSE)),
      column(4,selectInput("resModAll","Result(s)","",multiple=TRUE,size=10,selectize=FALSE))
    ),br(),
    conditionalPanel(condition="input.typeRes =='PDF'",
      HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
    ),
    br(),br(),shinyBS::bsAlert("alertResult")
  )
}
#------------------------------------------ refreshResults ------------------------------------------
#' @export
# Function to refresh the list with result files/folders
refreshResults <- function(inp,session,projloc="."){
  updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projloc,"/analysis"),recursive=FALSE,full.names=FALSE))
  if(inp$resModLst!="" && !is.null(inp$resModLst))  updateSelectInput(session,"resModAll",choices=list.files(paste(projloc,"/analysis/",inp$resModLst),pattern=ifelse(inp$typeRes=="PDF","\\.pdf$","\\.html$")))
}
#------------------------------------------ udpateResultList ------------------------------------------
#' @export
# Function to update the list with result files (in case new model is selected)
udpateResultList <- function(inp,session,projloc="."){
  #cat("doing something\n")
  if(inp$resModLst!="" && !is.null(inp$resModLst)) updateSelectInput(session,"resModAll",choices=list.files(paste0(projloc,"/analysis/",inp$resModLst),pattern=ifelse(inp$typeRes=="PDF","\\.pdf$","\\.html$")))
}
#------------------------------------------ changeResults ------------------------------------------
#' @export
# Function to change the list with result files (in case different extension is selected)
changeResults <- function(inp,session,projloc="."){
  if(inp$resModLst!="" && !is.null(inp$resModLst)) updateSelectInput(session,"resModAll",choices=list.files(paste0(projloc,"/analysis/",inp$resModLst),pattern=ifelse(inp$typeRes=="PDF","\\.pdf$","\\.html$")))
}
#------------------------------------------ showResults ------------------------------------------
#' @export
# Function to combine (if applicable) and show the results of the selected result files
showResults <- function(inp,session,projloc="."){
  shinyBS::closeAlert(session,"alertResultID")
  if(inp$resModLst!="" && !is.null(inp$resModLst) && !is.null(inp$resModAll)){
    if(inp$typeRes=="PDF"){
      if(length(inp$resModAll)==1){
        if(Sys.info()['sysname']=="Darwin"){
          try(system(paste0("open \"",projloc,"/analysis/",inp$resModLst,"/",inp$resModAll,"\""),wait=FALSE))
        }else if(Sys.info()['sysname']=="Linux"){
          try(system(paste0("xdg-open '",projloc,"/analysis/",inp$resModLst,"/",inp$resModAll,"'")))
        }else if(Sys.info()['sysname']=="Windows"){
          try(shell(paste0("\"",projloc,"/analysis/",inp$resModLst,"/",inp$resModAll,"\""),wait=FALSE))
        }
      }else{
        ftr <- paste0(projloc,"/analysis/",inp$resModLst,"/",sub("\\.pdf$",".tex.rawtex",inp$resModAll))
        ftr <- ftr[file.exists(ftr)]
        R3port::ltx_combine(list(ftr),out=paste0(projloc,"/analysis/",inp$resModLst,"/",inp$nameRes,".tex"),show=TRUE)
      }
    }else if(inp$typeRes=="HTML"){
      if(length(inp$resModAll)==1){
        if(Sys.info()['sysname']=="Darwin"){
          try(system(paste0("open '",normalizePath(paste0(projloc,"/analysis/",inp$resModLst,"/",inp$resModAll)),"'"),wait=FALSE))
        }else{
          utils::browseURL(paste0("file://",normalizePath(paste0(projloc,"/analysis/",inp$resModLst,"/",inp$resModAll))))
        }
      }else{
        ftr <- paste0(projloc,"/analysis/",inp$resModLst,"/",sub("\\.html$",".html.rawhtml",inp$resModAll))
        print(ftr)
        ftr <- ftr[file.exists(ftr)]
        print(ftr)
        R3port::html_combine(list(ftr),out=paste0(projloc,"/analysis/",inp$resModLst,"/",inp$nameRes,".html"),show=TRUE,
                             template=paste0(system.file(package="R3port"),"/bootstrap.html"))
      }
    }
  }else{
    shinyBS::createAlert(session,"alertResult",content="Select folder and results for report",append=FALSE,alertId="alertResultID",style="danger")
  }
}
