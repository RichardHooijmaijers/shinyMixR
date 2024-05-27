#------------------------------------------ module_overview_ui ------------------------------------------
#' Oerview module for UI
#'
#' @description Shiny module for overview
#'
#' @param id Module id
#' 
#' @export
module_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id='buttondiv', class='btn-group',
      actionButton(ns("overview_refr"), "Refresh",icon=icon("arrows-rotate")),
      module_metadata_ui(ns("adapt_meta_ov"),"overview"),
      module_scripts_ui(ns("runscripts")),
      module_reports_ui(ns("reports")),
      actionButton(ns("hlr"), "Results",icon=icon("file-lines")),
      actionButton(ns("del"), "Delete model(s)",icon=icon("trash"))
    ),br(),br(),
    box(width=NULL,title = span(id="projTitle1",span(id="projTitle2","Overview")), solidHeader = TRUE, status = "primary",collapsible = TRUE,
      DT::dataTableOutput(ns("overview_tbl"))
    ),
    box(width=NULL,title = "Tree View",solidHeader = TRUE, status = "primary",collapsible = TRUE,collapsed = TRUE,
      actionButton(ns("mktree"), "make tree",icon=icon("tree")),
      collapsibleTree::collapsibleTreeOutput(ns("treeout"))
    )
  )
}
#------------------------------------------ module_overview_server ------------------------------------------
#' Overview module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_overview_server <- function(id, r) {
  moduleServer(id, function(input, output, session){
    
    observe({
      
      print("change in r$proj_obj")
      
      # Make reactive value to hold the available models/scripts
      r$mdls <- list.files(paste0(r$this_wd, "/models"), pattern = "run[[:digit:]]*\\.[r|R]", full.names = TRUE)
      r$scrpt <- list.files(paste0(r$this_wd, "/scripts"), full.names = TRUE)
      
      # if no models are present in r$proj_obj, return empty table
      if (length(names(r$proj_obj)[names(r$proj_obj) != "meta"]) > 0) {
        r$overview_ov <- overview(r$proj_obj)
      } else {
        r$overview_ov <- data.frame(models="",importance="",description="",ref="",data="",method="",OBJF="",dOBJF=NA,runtime="")
      }
    })
    
    proxy = DT::dataTableProxy("overview_tbl")
    output$overview_tbl = DT::renderDataTable(r$overview_ov,rownames=FALSE,extension=c("Buttons"), options=list(scrollX=TRUE,dom="Bfrtip",buttons=c('colvis','csv'),pageLength=100,lengthMenu=c(10,100,1000,10000)))
    # filter="bottom", --> bug with filters/module/modal

    # Create tree
    tree <- eventReactive(input$mktree,{
      if(file.exists("shinyMixR")){tree_overview(r$proj_obj)}else{data.frame()}
    })
    output$treeout <- collapsibleTree::renderCollapsibleTree(tree())

    # Refresh overview
    observeEvent(input$overview_refr, {
      if(file.exists("shinyMixR")){
        r$proj_obj <- get_proj(r$this_wd)
        overview_ov <- overview(r$proj_obj)
        DT::replaceData(proxy, overview_ov, rownames = FALSE)
        r$mdls  <- list.files("models",pattern="run[[:digit:]]*\\.[r|R]",full.names = TRUE)
        r$scrpt <- list.files("scripts",full.names = TRUE)
      }
    },ignoreInit = TRUE)

    # Handle meta data (we need to pass the selected line as a reactive)
    selectedLine <- reactive({
      if (is.null(input$overview_tbl_rows_selected)) return(NULL) else return(input$overview_tbl_rows_selected)
    })
    upd <- module_metadata_server("adapt_meta_ov","overview",selline=selectedLine)
    observeEvent(upd(),{
       if(upd()=="Update DT") DT::replaceData(proxy, overview(r$proj_obj), rownames = FALSE)
    })

    # Show high level results
    resmodal <- function(){
      ns <- session$ns
      modalDialog(title="High level results",easyClose = TRUE,size="l",verbatimTextOutput(ns("res_out")))
    }
    hr_out <- eventReactive(input$hlr, {
      sel   <- sort(names(r$proj_obj)[names(r$proj_obj)!="meta"])[input$overview_tbl_rows_selected]
      if(length(sel)>0){
        res <- try(readRDS(paste0(r$this_wd,"/shinyMixR/",sel[1],".res.rds")))
        if(!"try-error"%in%class(res)) print(res) else print("No results available")
      }
    })
    output$res_out <- renderPrint(hr_out())
    observeEvent(input$hlr,{showModal(resmodal())},ignoreInit = TRUE)

    # Delete models
    delmodal <- function(){
      ns <- session$ns
      modalDialog(title="Delete model(s)",easyClose = TRUE,
                  checkboxInput(ns("delmodall"),"Delete all models and results",value=TRUE),
                  actionButton(ns("del2"), "Go",icon=icon("play")))
    }
    observeEvent(input$del,{showModal(delmodal())},ignoreInit = TRUE)
    observeEvent(input$del2,{
      if(!is.null(input$overview_tbl_rows_selected)){
        msel <- sort(names(r$proj_obj)[names(r$proj_obj)!="meta"])[input$overview_tbl_rows_selected]
        if(input$delmodall) {
          try(file.remove(paste0(r$this_wd,"/shinyMixR/",msel,".res.rds")))
          try(file.remove(paste0(r$this_wd,"/shinyMixR/",msel,".ressum.rds")))
          try(unlink(paste0(r$this_wd,"/analysis/",msel),recursive = TRUE))
        }
        try(file.remove(paste0(r$this_wd,"/models/",msel,".r")))
        r$proj_obj <- get_proj(r$this_wd)
        DT::replaceData(proxy, overview(r$proj_obj), rownames = FALSE)
        removeModal()
      }
    },ignoreInit = TRUE)

    module_scripts_server("runscripts", files = reactive(r$mdls), scripts = reactive(r$scrpt), loc = paste0(r$this_wd,"/shinyMixR/temp"))

    # Creating reports
    module_reports_server("reports")
  })
}  
