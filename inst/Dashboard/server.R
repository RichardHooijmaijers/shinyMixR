server <- function(input, output, session) {
  # Handling of tab switching (outside modules)
  r <- reactiveValues(swtab="",
                       model_updated = 0)
  observeEvent(input$tabs, r$swtab <- input$tabs)
  sett <- module_settings_server("settings")
  module_overview_server("oview")
  module_edit_server("editor",reactive(r$swtab),settings=sett)
  module_run_server("modrun",reactive(r$swtab), r = r)
  module_pt_server("partable",reactive(r$swtab), r = r)
  module_gof_server("gofplots",reactive(r$swtab),settings=sett)
  module_fitplots_server("fitplots",reactive(r$swtab),settings=sett)
  module_dataexplore_server("explore",reactive(r$swtab))
}