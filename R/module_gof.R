#------------------------------------------ module_gof_ui ------------------------------------------
#' GOF plots module for UI
#'
#' @description Shiny module for GOF plots
#'
#' @param id,input,output,session Internal parameters for {shiny}
#' 
#' @noRd
#' @export
module_gof_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width=3, title = "Settings", status="lightblue",solidHeader=TRUE, 
        actionButton(ns("make"), "Create plot",icon=icon("play")),
        actionButton(ns("save"), "Save plot",icon=icon("floppy-disk")),hr(),    
        selectInput(ns("gofLst"),"Model(s):",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE,size=5,selectize=FALSE),
        textInput(ns("subset"), "Subset:", value = "", placeholder="e.g. ID!=10"),
        textInput(ns("precode"), "Pre-code:", value = "",placeholder="e.g. dataIn$DV <- log(dataIn$DV)"),
        selectInput(ns("ptype"), "Type:", choices = c("all","ipred.dv","pred.dv","idv.res","pred.res")),
        checkboxInput(ns("linscale"), "Linear scale", value = FALSE),
        # selectInput(ns("by"),"Panel by:","",multiple=FALSE,size=5,selectize=FALSE), # maybe later...
        selectInput(ns("colby"),"Color by:","",multiple=FALSE),
        numericInput(ns("plheight"), "plot height:", 800)
        
      ),
      # When using a box, the content overflows, also a box does not provide a lot of functionality in this case
      # box(width=9, title = "Output",status="lightblue",solidHeader=TRUE,plotOutput(ns("gof_plot")),height="80vh") #,width="80%" ,height="100%"
      column(9,plotOutput(ns("gof_plot")))
    )
  )
}
#------------------------------------------ module_gof_server ------------------------------------------
#' GOF plots module for server
#' @param tabswitch reactive value that monitors the tabswitch
#' @param settings reactive value with the app settings
#' @noRd 
#' @export
module_gof_server <- function(id,tabswitch,settings) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(tabswitch(),{
      if(tabswitch()=="gof"){
        updateSelectInput(session, "gofLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$gofLst)
      }
    },ignoreInit=TRUE)
    
    # Adapt the selection of variables when model is selected
    observeEvent(input$gofLst,{
      datar <- try(readRDS(paste0("shinyMixR/",input$gofLst,".res.rds")))
      if(!"try-error"%in%class(datar)){
        updateSelectInput(session, "colby", choices = c("",names(datar)))
      }else{
        updateSelectInput(session, "colby", choices = "")
      } 
    })

    # Create GOF plot (type of plot taken from settings!)
    gofpl <- function(inp,saveit=FALSE){
      dataIn <- readRDS(paste0("shinyMixR/",inp$gofLst,".res.rds"))
      if(inp$subset!="")  dataIn <- subset(dataIn,eval(parse(text=input$subset)))
      if(inp$precode!="") eval(parse(text=input$precode))
      if(inp$colby=="")   clr <- NULL else clr <- inp$colby
      if(!saveit){
        gof_plot(dataIn,type=settings()$plott,colby=clr,ptype=inp$ptype,linscale=inp$linscale)  
      }else{
        savnm  <- ifelse(inp$typeout=="PDF",paste0(inp$savename,".tex"),paste0(inp$savename,".html"))
        gof_plot(dataIn,mdlnm=inp$gofLst,outnm=savnm,show=inp$showres,type=settings()$plott,colby=clr,ptype=inp$ptype,linscale=inp$linscale) 
        removeModal()
      }
    }
    plheight <- function() return(input$plheight)
    gofplm <- eventReactive(input$make,gofpl(input))
    output$gof_plot   = renderPlot(gofplm(),height=plheight)
    
    exportTestValues(
      plot_updated = gofplm()
    )

    # Save results - check if a module should be available here
    gofsave <- function(){
      ns <- session$ns
      modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
        textInput(ns("savename"),"Save as",value="GOF"),
        radioButtons(ns("typeout"), "Save type", choices = c("HTML","PDF"), inline = TRUE),
        checkboxInput(ns("showres"),"Show on save",value=FALSE),
        actionButton(ns("save2"), "Save",icon=icon("save")),br(),
        HTML("Modal will close when output is saved"),
        conditionalPanel(condition="input.typeout =='PDF'",
          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
        ,ns=ns)
      )
    }
    observeEvent(input$save,showModal(gofsave()))
    observeEvent(input$save2, gofpl(input,saveit=TRUE))
  })
}
