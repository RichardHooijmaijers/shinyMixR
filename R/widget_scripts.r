# The widget scripts are set-up to include at least a function that defines the UI using
# the tagList function. Furthermore specific server functions could be included. In general
# these functions typically has one or multiple of the following arguments (any other specific
# arguments are documented above the function itself):
# - projlst; project object with model information
# - inp; list with input elements from the shiny application (available in server as 'input')
# - session; the session object passed to the function given to shinyServer
# naming of the UI function is done by postfixing UI to the name of the widget
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ scriptsUI ------------------------------------------
#' @export
scriptsUI <- function() {
  tagList(
    fluidRow(
      column(3,actionButton("runScript", "Run script",icon=icon("play")),br(),br(),actionButton("showScript", "Show progress",icon=icon("spinner"))),
      column(4,selectInput("scriptModLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE)),
      column(4,selectInput("scriptFilLst","Files",list.files("./scripts"),multiple=FALSE,size=10,selectize=FALSE))
    ),
    br(),br(),shinyBS::bsAlert("alertScript"),br(),
    verbatimTextOutput("scriptProgrTxt"),
    tags$head(tags$style("#scriptProgrTxt{overflow-y:scroll; max-height: 600px;}"))
  )
}
#------------------------------------------ createRunScript ------------------------------------------
#' @export
# Function to create and run custom script
# save: logical indicating if the resulting plot should be saved
createRunScript <- function(inp,session,projloc="."){
  shinyBS::closeAlert(session,"alertScriptID")
  if(is.null(inp$scriptModLst) | is.null(inp$scriptFilLst)){
    shinyBS::createAlert(session,"alertScript",content="Select model and script to perform this action",append=FALSE,alertId="alertScriptID",style="danger")
  }else{
    dir.create(paste0(projloc,"/shinyMixR/temp"),showWarnings = FALSE,recursive = TRUE)
    scr   <- readLines(paste0(projloc,"/scripts/",inp$scriptFilLst))
    tmpsc <- paste0(projloc,"/shinyMixR/temp/",inp$scriptFilLst,".",stringi::stri_rand_strings(1,6),".r")
    writeLines(c(paste0("setwd('",projloc,"')"),paste0("models <- c(", paste(shQuote(inp$scriptModLst),collapse = ", "),")"),scr),tmpsc)
    writeLines(paste("Run",inp$scriptFilLst,"for model(s)",paste(inp$scriptModLst,collapse = ", "),"in",projloc),paste0(projloc,"/shinyMixR/temp/scriptres.out"))
    if(Sys.info()['sysname']=="Windows"){
      shell(paste0("Rscript \"", tmpsc,  "\" >> \"",projloc,"/shinyMixR/temp/scriptres.out\" 2>&1"),wait=FALSE)
    }else{
      system(paste0("Rscript \"", tmpsc,  "\" >> \"",projloc,"/shinyMixR/temp/scriptres.out\" 2>&1"),wait=FALSE)
    }
    shinyBS::createAlert(session,"alertScript",content=paste("Script",inp$scriptFilLst,"submitted"),append=FALSE,alertId="alertScriptID",style="success")
  }
}
#------------------------------------------ showScriptProgress ------------------------------------------
#' @export
# Function to read the script output file
showScriptProgress <- function(inp,session,projloc="."){
  scrlog   <- readLines(paste0(projloc,"/shinyMixR/temp/scriptres.out"))
  cat(scrlog,sep="\n")
}
