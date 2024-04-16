#------------------------------------------ module_fitplots_ui ------------------------------------------
#' Fit plots module for UI
#'
#' @description Shiny module for fit plots
#'
#' @param id Module id
#' @param proj_obj Project object
#' 
#' @export
module_fitplots_ui <- function(id, proj_obj) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width=3, title = "Settings", status="lightblue",solidHeader=TRUE, 
        actionButton(ns("make"), "Create plot",icon=icon("play")),
        actionButton(ns("save"), "Save plot",icon=icon("floppy-disk")),hr(),
        selectInput(ns("fitLst"),"Model(s):",names(proj_obj)[names(proj_obj)!="meta"],multiple=FALSE,size=5,selectize=FALSE),
        textInput(ns("subset"), "Subset:", value = "", placeholder="e.g. ID!=10"),
        textInput(ns("precode"), "Pre-code:", value = "",placeholder="e.g. dataIn$DV <- log(dataIn$DV)"),
        selectInput(ns("by"),"Panel by:","",multiple=TRUE),
        selectInput(ns("idv"),"Independent variable:","",multiple=FALSE),
        selectInput(ns("obs"),"Observed:","",multiple=FALSE),
        selectInput(ns("pred"),"Predicted:","",multiple=FALSE),
        selectInput(ns("ipred"),"Individual predicted:","",multiple=FALSE),
        selectInput(ns("grp"),"Grouping:","",multiple=FALSE),
        selectInput(ns("scales"),"Scales:",c("fixed", "free", "free_x", "free_y"),multiple=FALSE),
        checkboxInput(ns("logy"), "Logarithmic y scale", value = TRUE),
        numericInput(ns("plheight"), "plot height:", 800)        
      ),
      # When using a box, the content overflows, also a box does not provide a lot of functionality in this case
      column(9,plotOutput(ns("fit_plot")))
    )
  )


}
#------------------------------------------ module_fitplots_server ------------------------------------------
#' Fit plots module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' @param settings reactive value with the app settings
#' 
#' @export
module_fitplots_server <- function(id, r, settings) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(r$active_tab,{
      if(r$active_tab=="fitpl"){
        updateSelectInput(session, "fitLst", choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=input$fitLst)
      }
    },ignoreInit=TRUE)

    # Adapt the selection of variables when model is selected
    observeEvent(input$fitLst,{
      datar <- try(readRDS(paste0("shinyMixR/",input$fitLst,".res.rds")))
      if(!"try-error"%in%class(datar)){
        updateSelectInput(session, "by", choices = c("",names(datar)),selected="ID")
        updateSelectInput(session, "idv", choices = c("",names(datar)),selected="TIME")
        updateSelectInput(session, "obs", choices = c("",names(datar)),selected="DV")
        updateSelectInput(session, "pred", choices = c("",names(datar)),selected="PRED")
        updateSelectInput(session, "ipred", choices = c("",names(datar)),selected="IPRED")
        updateSelectInput(session, "grp", choices = c("",names(datar)),selected="ID")
      }else{
        for(i in c("by", "idv", "obs", "pred", "ipred", "grp")) updateSelectInput(session, i, choices = "")
      } 
    })

    # Create fit plot (type of plot taken from settings!)
    fitpl <- function(inp,saveit=FALSE){
      #cat("got clicked\n")
      dataIn <- readRDS(paste0("shinyMixR/",input$fitLst[1],".res.rds"))
      if(inp$subset!="")     dataIn <- subset(dataIn,eval(parse(text=input$subset)))
      if(inp$precode!="")    eval(parse(text=input$precode))
      if(!isTruthy(inp$by))  byr <- NULL else byr <- inp$by
      if(inp$ipred=="")      iprr <- NULL else iprr <- inp$ipred
      if(!saveit){
        fit_plot(dataIn,type=settings()$plott,by=byr,idv=inp$idv,obs=inp$obs,pred=inp$pred,ipred=iprr,grp=inp$grp,logy=inp$logy,scales=inp$scales) 
      }else{
        savnm  <- ifelse(inp$typeout=="PDF",paste0(inp$savename,".tex"),paste0(inp$savename,".html"))
        fit_plot(dataIn,mdlnm=inp$fitLst,outnm=savnm,show=inp$showres, type=settings()$plott,by=byr,idv=inp$idv,obs=inp$obs,pred=inp$pred,ipred=iprr,grp=inp$grp,logy=inp$logy,scales=inp$scales) 
        removeModal()
      }
    }
    plheight <- function() return(input$plheight)
    fitplm <- eventReactive(input$make,fitpl(input))
    output$fit_plot   = renderPlot(fitplm(),height=plheight)

    # Save results - check if a module should be available here
    fitsave <- function(){
      ns <- session$ns
      modalDialog(title="Save results",easyClose = TRUE,fade=FALSE,
        textInput(ns("savename"),"Save as",value="fits"),
        radioButtons(ns("typeout"), "Save type", choices = c("HTML","PDF"), inline = TRUE),
        checkboxInput(ns("showres"),"Show on save",value=FALSE),
        actionButton(ns("save2"), "Save",icon=icon("save")),br(),
        HTML("Modal will close when output is saved"),
        conditionalPanel(condition="input.typeout =='PDF'",
          HTML("<strong style='color: red;'>Latex including various packages is needed to create PDF output</strong>")
        ,ns=ns)
      )
    }
    observeEvent(input$save,showModal(fitsave()))
    observeEvent(input$save2, fitpl(input,saveit=TRUE))
  })
}
