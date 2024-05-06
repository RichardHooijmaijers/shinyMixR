#------------------------------------------ run_shinymixr ------------------------------------------
#' Creates and run the interface
#'
#' @param wd character with the working directory
#' @param dry_run logical, if TRUE, the function will not launch the app, but will only create the necessary files
#' @param ... arguments passed to the shiny runApp function
#' @importFrom shiny runApp HTML NS br checkboxGroupInput checkboxInput conditionalPanel
#' div em eventReactive exportTestValues fluidRow hr icon
#' insertUI isTruthy isolate modalDialog moduleServer
#' numericInput observe observeEvent plotOutput radioButtons reactive
#' reactivePoll reactiveVal reactiveValues reactiveValuesToList
#' removeModal removeUI renderPlot renderPrint renderText req
#' selectInput showModal sliderInput span tabPanel tagList
#' tags textInput updateSelectInput updateSliderInput updateTabsetPanel
#' updateTextInput verbatimTextOutput
#' @import bs4Dash ggplot2 gridExtra
#' @export
#' @return runs the shinyMixR interface
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  run_shinymixr(".")
#' }
run_shinymixr <- function(...){ # wd = getwd(), dry_run = FALSE,
  
  #if(!file.exists(paste0(wd,"/shinyMixR/app/www"))) try(dir.create(paste0(wd,"/shinyMixR/app/www"),recursive = TRUE))
  #if(!file.exists(paste0(wd,"/shinyMixR/app/shinyMixR/temp")))    try(dir.create(paste0(wd,"/shinyMixR/app/shinyMixR/temp"),recursive=TRUE))
  #if(!file.exists(paste0(wd,"/shinyMixR/temp")))    try(dir.create(paste0(wd,"/shinyMixR/temp"),recursive=TRUE))
  
  #try(file.copy(system.file("dashboard","app.R",package="shinyMixR"),          paste0(wd,"/shinyMixR/app/app.R"),overwrite = TRUE),silent = TRUE)
  #try(file.copy(system.file("dashboard","www/logoshinyMixR.png",package="shinyMixR"), paste0(wd,"/shinyMixR/app/www/logoshinyMixR.png")),silent = TRUE)
  
  # Set the working directory so the project can be found
  #adpt <- readLines(system.file("dashboard", "app.R", package = "shinyMixR"))
  #adpt <- c(paste0("setwd(\"", normalizePath(wd, winslash = "/"), "\")"), adpt)
  #writeLines(adpt, paste0(wd,"/shinyMixR/app/app.R"))
  
  # Clean up stuff before running the app (check if feasible or not)
  #try(unlink(list.files(paste0(wd,"/shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE)))
  # if (dry_run == TRUE) {
  #   return()
  # } else {
  #   shiny::runApp(paste0(wd,"/shinyMixR/app"),...)
  # }

  if(!file.exists("shinyMixR/temp")) try(dir.create("shinyMixR/temp",recursive=TRUE))
  proj_obj <- get_proj()
  # Check and load nlmixr(2)
  if ("nlmixr2" %in% rownames(installed.packages())){
    library(nlmixr2)
  } else {
    cat("you need the 'nlmixr2' package to run models\n")
  }

  newtheme <- fresh::create_theme(
    theme = "darkly", # theme has no effect, at least within bs4Dash
    fresh::bs4dash_font(size_base = "0.9rem"),
    fresh::bs4dash_status(primary = "#3c8dbc")
  )

  shinyApp(
    ui = dashboardPage(
      title = "shinyMixR",
      # Header
      header = dashboardHeader(
        title = dashboardBrand(title = "ShinyMixR", color = "lightblue"), #, color = "lightblue", href = "#", image = "logoshinyMixR.png"),
        leftUI = tags$img(src=paste0("data:image/png;base64,",xfun::base64_encode(system.file("dashboard/www/logoshinyMixR.png", package = "shinyMixR"))),height=40)
      ),
      # Sidebar menu
      sidebar = dashboardSidebar(status="lightblue", elevation = 1,
                                  sidebarMenu(id="tabs", 
                                    menuItem('Model overview', tabName='overview', icon=icon('table')),
                                    menuItem('Edit model(s)', tabName='editor', icon=icon('file-pen')),
                                    menuItem('Run model(s)', tabName='run', icon=icon('person-running')),
                                    menuItem('Parameter estimates', tabName='par', icon=icon('table-cells')),
                                    menuItem('Goodness of fit', tabName='gof', icon=icon('chart-line')),
                                    menuItem('Fit plots', tabName='fitpl', icon=icon('chart-line')),
                                    menuItem('Data exploration', tabName='expl', icon=icon('magnifying-glass')),
                                    menuItem('Settings', tabName='settings', icon=icon('gear'))
                                  )
      ),
      # Main body
      body = dashboardBody(
        # First set theme and include css
        fresh::use_theme(newtheme),
        # CHECK IF THE LINES BELOW WILL WORK WITHOUT INTERNET CONNECTION AND DOES IT FAIL GRACEFULLY?!
        shinyWidgets::useSweetAlert("minimal"),
        tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
        tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
        tags$head(tags$style("#progph{overflow-y:scroll; max-height: 600px;}")),
        tags$head(tags$style(HTML("#exploretabout{height: 75vh; overflow-y: auto;}"))),
        tags$head(tags$style(HTML(".swal2-popup {font-size: 0.9rem !important;}"))),    
        tags$head(tags$style(HTML("input[id$=\"subset\"]{font-family:\"Courier New\"}"))),
        tags$head(tags$style(HTML("input[id$=\"precode\"]{font-family:\"Courier New\"}"))),
        tags$head(tags$style(HTML("label{margin-bottom:0rem;}"))),
        tabItems(
          tabItem(tabName = "overview", module_overview_ui("oview")),
          tabItem(tabName = "editor", module_edit_ui("editor")),
          tabItem(tabName = "run", module_run_ui("modrun", proj_obj)),
          tabItem(tabName = "par", module_pt_ui("partable", proj_obj)),
          tabItem(tabName = "gof", module_gof_ui("gofplots", proj_obj)),
          tabItem(tabName = "fitpl", module_fitplots_ui("fitplots", proj_obj)),
          tabItem(tabName = "expl", module_dataexplore_ui("explore")),
          tabItem(tabName = "settings", module_settings_ui("settings"))
        )  
      )
    ),
    server = function(input, output, session) {
       # Top-level reactive values
      r <- reactiveValues(active_tab = "",
                          model_updated = 0,
                          proj_obj = get_proj(),
                          this_wd = ".")
      
      observeEvent(input$tabs, r$active_tab <- input$tabs)
      
      # Modules
      sett <- module_settings_server("settings")
      module_overview_server("oview", r = r)
      module_edit_server("editor", r = r, settings=sett)
      module_run_server("modrun", r = r)
      module_pt_server("partable", r = r)
      module_gof_server("gofplots", r = r,settings=sett)
      module_fitplots_server("fitplots", r = r,settings=sett)
      module_dataexplore_server("explore", r = r)

    },
    options = list(launch.browser=TRUE,...) # set general options here for running the app
  )
  


}
