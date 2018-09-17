# Initially shiny modules were used. However this made things quite complex in combination with DT:
# https://github.com/rstudio/DT/issues/359
# This is also true for other items (shinyBS and shinyAce) therefore everything was placed in functions instead
# The .cwd is necessary as the working directory should be different from the ui/server (the usage run_shinymixr is key to work here)
# a shinyMixR and temp subfolder are created as it is necessary to place stuff in
# the proj_obj is assigned in the global environment to easily switch between the interface and CLI
setwd(.cwd)
if(file.exists("shinyMixR")){assign("proj_obj",get_proj(),pos = .GlobalEnv)}else{assign("proj_obj",list(),pos = .GlobalEnv)}
assign("projwd",getwd(),pos = .GlobalEnv)

shinyUI( shinydashboard::dashboardPage(

  # Header of the app
  title = "shinyMixR",
  shinydashboard::dashboardHeader(
    title = tags$img(src='logoSmall2.png',height=45),
    tags$li(img(src = 'logo2.png',height=42),class = "dropdown")
  ),

  # Sidebar items
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Model overview", tabName = "tabOverview", icon = icon("folder")),
      shinydashboard::menuItem("Widgets", icon = icon("th"),startExpanded = TRUE,
        shinydashboard::menuSubItem("Edit model(s)", tabName = "tabEdit"),
        shinydashboard::menuSubItem("Run model(s)", tabName = "tabRun"),
        shinydashboard::menuSubItem("Parameter estimates", tabName = "tabParEst"),
        shinydashboard::menuSubItem("Goodness of fit", tabName = "tabGOF"),
        shinydashboard::menuSubItem("Fit plots", tabName = "tabFit"),
        shinydashboard::menuSubItem("Scripts", tabName = "tabScripts"),
        shinydashboard::menuSubItem("Analysis results", tabName = "tabRes")
      ),
      shinydashboard::menuItem("Settings",tabName = "tabSettings", icon = icon("cog"))
    ),
    textOutput("res")
  ),

  # body of the app
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "tabOverview",overviewUI(proj_obj)),
      shinydashboard::tabItem(tabName = "tabEdit",editUI(proj_obj)),
      shinydashboard::tabItem(tabName = "tabRun",runUI()),
      shinydashboard::tabItem(tabName = "tabParEst",partableUI()),
      shinydashboard::tabItem(tabName = "tabGOF",gofUI()),
      shinydashboard::tabItem(tabName = "tabFit",fitUI()),
      shinydashboard::tabItem(tabName = "tabScripts",scriptsUI()),
      shinydashboard::tabItem(tabName = "tabRes",resUI()),
      # Settings tab
      shinydashboard::tabItem(tabName = "tabSettings",
        selectInput("plotType","Plot types",c("xpose","user"),multiple=FALSE),
        sliderInput("fontEditor","Font size editor",min=6,max=18,value=14,step=1),
        selectInput("themeEditor","Editor theme",shinyAce::getAceThemes(),selected="solarized_light",multiple=FALSE),
        checkboxInput("createsmf","Create shinyMixR folders after project selection",value=TRUE)
        #numericInput("plotHgt","Plot height",800)
      )
    )
  )
))
