# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="script",
             title="Scripts",
             icon="terminal")

widgetui <- function() {
  tagList(
    actionButton("script_run", "Run script",icon=icon("play")),br(),br(),
    fluidRow(
      column(4,selectInput("script_modLst","Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE)),
      column(4,selectInput("script_scrLst","Files",list.files("scripts"),multiple=FALSE,size=10,selectize=FALSE))
    ),
    br(),br(),
    verbatimTextOutput("script_ProgrTxt"),
    tags$head(tags$style("#script_ProgrTxt{overflow-y:scroll; max-height: 600px;}"))
  )
}
widgetserver <- function(input,output,session){

  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="script"){
      updateSelectInput(session, "script_modLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$script_modLst)
      updateSelectInput(session, "script_scrLst", choices = list.files("scripts"),selected=input$script_scrLst)
    }
  },ignoreInit=TRUE)

  observeEvent(input$script_run,{
    if(is.null(input$script_modLst) | is.null(input$script_scrLst)){
        shinyWidgets::sendSweetAlert(session = session, title="Run",text = "Select model and script to perform this action" ,type = "error")
      }else{
        dir.create(paste0("shinyMixR/temp"),showWarnings = FALSE,recursive = TRUE)
        scr   <- readLines(paste0("scripts/",input$script_scrLst))
        tmpsc <- paste0("shinyMixR/temp/",input$script_scrLst,".",stringi::stri_rand_strings(1,6),".r")
        writeLines(c(paste0("setwd('",normalizePath("."),"')"),paste0("models <- c(", paste(shQuote(input$script_modLst),collapse = ", "),")"),scr),tmpsc)
        writeLines(paste("Run",input$script_scrLst,"for model(s)",paste(input$script_modLst,collapse = ", "),"in",normalizePath(".")),paste0("shinyMixR/temp/scriptres.out"))
        if(Sys.info()['sysname']=="Windows"){
          shell(paste0("Rscript \"", tmpsc,  "\" >> \"",normalizePath("."),"/shinyMixR/temp/scriptres.out\" 2>&1"),wait=FALSE)
        }else{
          system(paste0("Rscript \"", tmpsc,  "\" >> \"",normalizePath("."),"/shinyMixR/temp/scriptres.out\" 2>&1"),wait=FALSE)
        }
        shinyWidgets::sendSweetAlert(session = session, title="Run",text = paste("Script",input$script_scrLst,"submitted") ,type = "success")
      }
  })

  # Show output
  runscrmonit            <- reactiveFileReader(500, session, "shinyMixR/temp/scriptres.out", readLines)
  output$script_ProgrTxt <- renderText(paste(runscrmonit(),collapse="\n"))
}
