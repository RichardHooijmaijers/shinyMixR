#------------------------------------------ shinymixr_gadget ------------------------------------------
#' Rstudio gadget to select project and start app
#'
#' @importFrom shiny hr div markdown textInput checkboxInput observeEvent
#' updateTextInput stopApp paneViewer
#' @import rstudioapi shinyFiles miniUI
#' @export
#' @return runs the shinyMixR interface
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  shinymixr_gadget()
#' }
shinymixr_gadget <- function(){ 
  if(!"shinymixr" %in% tolower((.packages()))){
    return("Gadget can only be used if shinyMixR package is loaded: library(shinyMixR)")
  }
  # --- UI part ---
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Run shinyMixR",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      hr(),div(markdown("## Introduction"),style="text-align: center"),
      markdown(c("This app let's you start the shinyMiXR interface.",
                "Below you can set a path (defaults to current working directory).",
                "The path can be either typed in the text box or a file browser can be used to search the path.",
                "This path is used as the root location where your project structure with models is present.",
                "You can select to create a project structure in case it is not yet present",
                "**When path and options are set, the app can be started by clicking the button below**")),hr(),
      textInput("path",label = "Path to start shinyMixR instance:",value = normalizePath(getwd(),winslash = "/"),width="100%"),
      shinyFiles::shinyDirButton("dirsel", label="Select folder", title='Please select a folder', multiple=FALSE),
      checkboxInput("crea",label = "Create project structure when not present",value = TRUE),
      padding = 25
    ),
    miniUI::miniButtonBlock(actionButton("go", "Start app"))
  )

  # --- Server part ---
  server <- function(input, output, session) {
    
    # Handle directory selection
    volumes <- c(shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "dirsel",root=volumes)
    observeEvent(input$dirsel,{
      if(!is.integer(input$dirsel)){
        updateTextInput(session,"path",value=shinyFiles::parseDirPath(volumes, input$dirsel))
      }
    })
    
    # Handle starting App (need rstudioapi to enable to run app from within app!)
    observeEvent(input$go,{
      if(dir.exists(input$path)){
        if(input$crea){
          try(shinyMixR::create_proj(input$path))
        }
        command <- paste0("shinyMixR::run_shinymixr(wd='",normalizePath(input$path, winslash = "/"),"')")
        rstudioapi::sendToConsole(command)
        stopApp()
      }else{
        shinyMixR::myalert("Folder does not exists",type = "error")
      }
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(400))
  
}
