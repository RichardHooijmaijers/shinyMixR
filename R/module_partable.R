#------------------------------------------ module_pt_ui ------------------------------------------
#' Parameter table module for UI
#'
#' @description Shiny module for parameter table
#'
#' @param id,input,output,session Internal parameters for {shiny}
#' 
#' @noRd
#' @export
module_pt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("savePars"), "Save parameter table",icon=icon("floppy-disk")),br(),br(),
    fluidRow(
      box(width=3, title = "Settings", status="lightblue",solidHeader=TRUE, 
        selectInput(ns("EstLst"),"Model(s)",names(proj_obj)[names(proj_obj)!="meta"],multiple=TRUE,size=10,selectize=FALSE),
        checkboxInput(ns("bsv"),"Include BSV",TRUE),
        checkboxInput(ns("shrink"),"Include shrinkage",TRUE),
        checkboxInput(ns("backt"),"Back-transformed parameters",FALSE)
      ),
      column(9,DT::dataTableOutput(ns("EstTbl")))
    )  
  )
}
#------------------------------------------ module_pt_server ------------------------------------------
#' Parameter table module for server
#' @param tabswitch reactive value that monitors the tabswitch
#' @param r reactive values object that is defined top-level
#' @noRd 
#' @export
module_pt_server <- function(id, tabswitch, r) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(tabswitch(),{
      if(tabswitch()=="par"){
        updateSelectInput(session, 
                          "EstLst", 
                          choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],
                          selected= ifelse(is.null(input$EstLst), names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"][1], input$EstLst) 
        )
      }
    },ignoreInit=TRUE)

    # Create parameter table
    parTable <- function(inp,projloc=".",saveit=FALSE){
      obj     <- get_proj(projloc=projloc)
      if(!saveit){
        #print(obj)
        #print(inp$EstLst)
        par_table(obj,models=inp$EstLst,bsv=inp$bsv,shrink=inp$shrink,backt=inp$backt,formatting=TRUE)
      }else{
        savnm  <- ifelse(inp$typePars=="PDF",paste0(inp$namePars,".tex"),paste0(inp$namePars,".html"))
        #print(savnm)
        par_table(obj,models=inp$EstLst,outnm=savnm,show=inp$showPars,projloc=projloc,bsv=inp$bsv,shrink=inp$shrink,backt=inp$backt,formatting=ifelse(inp$typePars=="PDF",FALSE,TRUE))
      }
    }
    
    output$EstTbl <- DT::renderDataTable({
      
      req(r$model_updated)
      
      table <- parTable(input)
      r$params <- table
      
      DT::datatable(
        table,
        rownames=FALSE,
        options=list(paging=FALSE,searching=FALSE),
        escape=FALSE,
        caption = tags$caption(style = "caption-side: bottom;",em("Table shows by default the final estimate and the %RSE in square brackets. In case BSV is checked, it will be added in curly braces as CV%. In case shrinkage is checked it will be added after the BSV. In case back-transformed parameters, the estimate is back-transformed and the 95% CI is added in parenthesis"))
      )
    })
    
    exportTestValues(
      params = r$params
    )

    # Save results
    parsave <- function(){
      ns <- session$ns
      modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
        textInput(ns("namePars"),"Save as",value="ParTable"),
        radioButtons(ns("typePars"), "Save type", choices = c("HTML","PDF"), inline = TRUE),
        checkboxInput(ns("showPars"),"Show on save",value=FALSE),
        actionButton(ns("savePars2"), "Save",icon=icon("floppy-disk")),br(),
        HTML("Modal will close when output is saved"),
        conditionalPanel(condition="input.typePars =='PDF'",
          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
        ,ns=ns)
      )
    }
    observeEvent(input$savePars,showModal(parsave()))
    observeEvent(input$savePars2, parTable(input,saveit=TRUE))
  })
}
