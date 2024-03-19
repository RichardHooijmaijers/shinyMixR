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
      #actionButton(ns("overview_adpt"), "Adapt model notes",icon=icon("list")),
      module_metadata_ui(ns("adapt_meta_ov"),"overview"),
      module_scripts_ui(ns("runscripts")),
      module_reports_ui(ns("reports")),
      #module_metadata_ui(ns("dummy"),"save"),
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
#' 
#' @export
module_overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    # Make reactive value to hold the available models/scripts
    rv <- reactiveValues(mdls=list.files("models",pattern="run[[:digit:]]*\\.[r|R]",full.names = TRUE),scrpt=list.files("scripts",full.names = TRUE))
    
    # Create overview when app is loaded 
    if(all(file.exists(c("analysis","data","models","scripts","shinyMixR")))){
      overview_ov <- overview()
    }else{
      overview_ov <- data.frame(models="",importance="",description="",ref="",data="",method="",OBJF="",dOBJF=NA,runtime="")
    }
    proxy = DT::dataTableProxy("overview_tbl")
    output$overview_tbl = DT::renderDataTable(overview_ov,rownames=FALSE,extension=c("Buttons"), options=list(scrollX=TRUE,dom="Bfrtip",buttons=c('colvis','csv'),pageLength=100,lengthMenu=c(10,100,1000,10000)))
    # filter="bottom", --> bug with filters/module/modal

    # Create tree
    tree <- eventReactive(input$mktree,{
      if(file.exists("shinyMixR")){tree_overview()}else{data.frame()}
    })
    output$treeout <- collapsibleTree::renderCollapsibleTree(tree())

    # Refresh overview
    observeEvent(input$overview_refr,{
      if(file.exists("shinyMixR")){
        assign("proj_obj",get_proj(),pos = .GlobalEnv)
        overview_ov <- overview()
        DT::replaceData(proxy, overview_ov, rownames = FALSE)
        rv$mdls  <- list.files("models",pattern="run[[:digit:]]*\\.[r|R]",full.names = TRUE)
        rv$scrpt <- list.files("scripts",full.names = TRUE)
      }
    },ignoreInit = TRUE)

    # Handle meta data (we need to pass the selected line as a reactive)
    selectedLine <- reactive({
      if (is.null(input$overview_tbl_rows_selected)) return(NULL) else return(input$overview_tbl_rows_selected)
    })
    upd <- module_metadata_server("adapt_meta_ov","overview",selline=selectedLine)
    observeEvent(upd(),{
       if(upd()=="Update DT") DT::replaceData(proxy, overview(), rownames = FALSE)
    })

    # Show high level results
    resmodal <- function(){
      ns <- session$ns
      modalDialog(title="High level results",easyClose = TRUE,size="l",verbatimTextOutput(ns("res_out")))
    }
    hr_out <- eventReactive(input$hlr, {
      sel   <- sort(names(proj_obj)[names(proj_obj)!="meta"])[input$overview_tbl_rows_selected]
      if(length(sel)>0){
        res <- try(readRDS(paste0("shinyMixR/",sel[1],".res.rds")))
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
        msel <- sort(names(proj_obj)[names(proj_obj)!="meta"])[input$overview_tbl_rows_selected]
        if(input$delmodall) {
          try(file.remove(paste0("shinyMixR/",msel,".res.rds")))
          try(file.remove(paste0("shinyMixR/",msel,".ressum.rds")))
          try(unlink(paste0("analysis/",msel),recursive = TRUE))
        }
        try(file.remove(paste0("models/",msel,".r")))
        assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
        DT::replaceData(proxy, overview(), rownames = FALSE)
        removeModal()
      }
    },ignoreInit = TRUE)

    # Running scripts - Check creation of temp folder (should be done in create_proj (get_proj) function?)
    dir.create("shinyMixR/temp",showWarnings = FALSE,recursive = TRUE)
    module_scripts_server("runscripts", files = reactive(rv$mdls), scripts = reactive(rv$scrpt), loc = "shinyMixR/temp")

    # Creating reports
    module_reports_server("reports")
  })
}  
