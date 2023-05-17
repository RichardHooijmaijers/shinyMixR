# We need the code below in the ui because of setting the working directory 
assign("proj_obj",get_proj(),pos = .GlobalEnv)
dashboardPage(
  title = "shinyMixR",
  # skin = "orange", 
  # controlbar = dashboardControlbar(skinSelector(), pinned = TRUE),
  # Header
  header = dashboardHeader(
    title = dashboardBrand(title = "ShinyMixR", color = "lightblue"), #, color = "lightblue", href = "#", image = "logoshinyMixR.png"),
    leftUI = tags$img(src='logoshinyMixR.png',height=40)
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
    use_theme(newtheme),
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
      tabItem(tabName = "run", module_run_ui("modrun")),
      tabItem(tabName = "par", module_pt_ui("partable")),
      tabItem(tabName = "gof", module_gof_ui("gofplots")),
      tabItem(tabName = "fitpl", module_fitplots_ui("fitplots")),
      tabItem(tabName = "expl", module_dataexplore_ui("explore")),
      tabItem(tabName = "settings", module_settings_ui("settings"))
    )  
  )
)

