# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="settings",
             title="Settings",
             icon="cog")

widgetui <- function() {
  tagList(
    # Currently the settings only have effect when changed within session
    # (future adaptations could be to make settings persistent)
    selectInput("settings_plott","Plot types",c("xpose","user"),multiple=FALSE),
    sliderInput("settings_fontedt","Font size editor",min=6,max=18,value=14,step=1),
    selectInput("settings_themeedt","Editor theme",shinyAce::getAceThemes(),selected="solarized_light",multiple=FALSE)
  )
}
widgetserver <- function(input,output,session){
  observeEvent(input$settings_fontedt,shinyAce::updateAceEditor(session, "editor_editor", fontSize=input$settings_fontedt))
  observeEvent(input$settings_themeedt,shinyAce::updateAceEditor(session, "editor_editor", theme=input$settings_themeedt))
}
