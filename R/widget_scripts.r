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
      column(2,actionButton("runScript", "Run script",icon=icon("play"))),
      column(3,selectInput("scriptModLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE)),
      column(3,selectInput("scriptFilLst","Files",list.files("./scripts"),multiple=FALSE,size=10,selectize=FALSE))
    ),
    br(),br(),shinyBS::bsAlert("alertScript")
  )
}
#------------------------------------------ createRunScript ------------------------------------------
#' @export
# Function to create and save a gof plots
# save: logical indicating if the resulting plot should be saved
createRunScript <- function(inp,session){
  scr   <- readLines(paste0("./scripts/",inp$scriptFilLst))
  tmpsc <- paste0("./shinyMixR/temp/",inp$scriptFilLst,".",stringi::stri_rand_strings(1,6),".r")
  writeLines(c(paste0("models <- c(", paste(shQuote(inp$scriptModLst),collapse = ", "),")"),scr),tmpsc)
  if(Sys.info()['sysname']=="Windows"){
    shell(paste0("Rscript ", tmpsc,  " > ",tmpsc,".out 2>&1"),wait=FALSE)
  }else{
    system(paste0("Rscript ", tmpsc,  " > ",tmpsc,".out 2>&1"),wait=FALSE)
  }
  shinyBS::createAlert(session,"alertScript",content=paste("Script",inp$scriptFilLst,"submitted"),append=FALSE,alertId="alertScriptID",style="success")
}
