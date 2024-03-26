server <- function(input, output, session) {
  # Top-level reactive values
  r <- reactiveValues(active_tab = "",
                      model_updated = 0)
  observeEvent(input$tabs, r$active_tab <- input$tabs)
  sett <- module_settings_server("settings")
  module_overview_server("oview")
  module_edit_server("editor", r = r, settings=sett)
  module_run_server("modrun", r = r)
  module_pt_server("partable", r = r)
  module_gof_server("gofplots", r = r,settings=sett)
  module_fitplots_server("fitplots", r = r,settings=sett)
  module_dataexplore_server("explore", r = r)
}