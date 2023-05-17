server <- function(input, output, session) {
  # Handling of tab switching (outside modules)
  rv <- reactiveValues(swtab="")
  observeEvent(input$tabs, rv$swtab <- input$tabs)
  sett <- module_settings_server("settings")
  module_overview_server("oview")
  module_edit_server("editor",reactive(rv$swtab),settings=sett)
  module_run_server("modrun",reactive(rv$swtab))
  module_pt_server("partable",reactive(rv$swtab))
  module_gof_server("gofplots",reactive(rv$swtab),settings=sett)
  module_fitplots_server("fitplots",reactive(rv$swtab),settings=sett)
  module_dataexplore_server("explore",reactive(rv$swtab))
}