#------------------------------------------ module_gof_ui ------------------------------------------
#' GOF plots module for UI
#'
#' @description Shiny module for GOF plots
#'
#' @param id Module id
#' @param proj_obj Project object
#' 
#' @export
module_gof_ui <- function(id, proj_obj) {
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
      column(9,plotOutput(ns("gof_plot")))
    )
  )
}
#------------------------------------------ module_gof_server ------------------------------------------
#' GOF plots module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' @param settings reactive value with the app settings
#' 
#' @export
module_gof_server <- function(id, r, settings) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(r$active_tab,{
      if(r$active_tab=="gof"){
        updateSelectInput(session, "gofLst", choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=input$gofLst)
      }
    },ignoreInit=TRUE)
    
    # Adapt the selection of variables when model is selected
    observeEvent(input$gofLst,{
      
      if (!file.exists(paste0(r$this_wd,"/shinyMixR/",input$gofLst,".res.rds"))) {
        updateSelectInput(session, "colby", choices = "")
      } else {
        datar <- readRDS(paste0(r$this_wd,"/shinyMixR/",input$gofLst,".res.rds"))
        updateSelectInput(session, "colby", choices = c("",names(datar)))
      }
    })
    
    # Create GOF plot (type of plot taken from settings!)
    gofpl <- function(inp,saveit=FALSE){
      dataIn <- readRDS(paste0(r$this_wd,"/shinyMixR/",inp$gofLst,".res.rds"))
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
    
    # Get the plot data for testing purposes - only test individual plots
    plot_updated <- reactive({
      if (input$ptype != "all") {
        # elements to retrieve
        el <- c("x", "y")
        plot_data <- ggplot_build(gofplm())$data[[1]][el]
        # remove NA or -Inf rows (dirty try-to-fix GHA)
        plot_data[sapply(plot_data, is.infinite)] <- NA
        plot_data <- na.omit(plot_data)
        # remove rows where value is > - 10 (again, dirty try-to-fix GHA)
        plot_data <- plot_data[plot_data$y > -10, ]
        plot_data <- plot_data[plot_data$x > -10, ]
        # round to 6 decimals - different rounding on different OS systems
        plot_data$x <- sprintf("%.6f", round(plot_data$x, digits = 6))
        plot_data$y <- sprintf("%.6f", round(plot_data$y, digits = 6))
        # replace -0.000000 with 0.000000
        plot_data[plot_data == "-0.000000"] <- "0.000000"
        return(plot_data)
      } else {
        return(NULL)
      }
    })
    
    exportTestValues(
      plot_updated = plot_updated()
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
