#------------------------------------------ module_settings_ui ------------------------------------------
#' Settings module for UI
#'
#' @description Shiny module for settings
#'
#' @param id Module id
#' 
#' @export
module_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Currently the settings only have effect when changed within session
    # (future adaptations could be to make settings persistent)
    selectInput(ns("plott"),"Plot types",c("user","xpose"),multiple=FALSE),
    sliderInput(ns("fontedt"),"Font size editor",min=6,max=18,value=14,step=1),
    selectInput(ns("themeedt"),"Editor theme",shinyAce::getAceThemes(),selected="solarized_light",multiple=FALSE)
  )
}
#------------------------------------------ module_settings_server ------------------------------------------
#' Settings module for server
#' 
#' @param id Module id
#' 
#' @export
module_settings_server <- function(id) {
  moduleServer(id,function(input, output, session) {
    ret <- reactive({reactiveValuesToList(input)})
    return(ret)
  })
}
