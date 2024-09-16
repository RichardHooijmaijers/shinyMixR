#------------------------------------------ module_dataexplore_ui ------------------------------------------
#' Data exploration module for UI
#'
#' @description Shiny module for data exploration
#'
#' @param id Module id
#' 
#' @export
module_dataexplore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("make"),"Create plots",icon=icon("play")),
    actionButton(ns("maketbl"),"Create table",icon=icon("play")),
    actionButton(ns("makeply"),"Create plotly",icon=icon("play")),
    actionButton(ns("clearfields"),"Clear fields",icon=icon("trash")),
    br(),br(),
    fluidRow(
      tabBox(width=4,
        title = "",
        id = ns("exploretabs"),
        tabPanel("Data",
          selectInput(ns("mdls"), "Models:", choices = "",multiple=FALSE,selectize =FALSE,size=10,selected=NA),
          checkboxInput(ns("use_input"),  "Use input dataset", value = FALSE)
        ),
        # The base layer
        tabPanel("Base Layer",
          fluidRow(
            column(6,
              selectInput(ns("Xval1"),   "X value:", choices = ""),
              selectInput(ns("Yval1"),   "Y value:", choices = ""),
              selectInput(ns("geoms1"),  "Geom:", choices = c("[empty]","point","line","boxplot","bar","histogram","smooth","jitter","text")),
              selectInput(ns("stats1"),  "Stat:", choices = c("[empty]","mean","median","mean (SD)","median (5-95th perc.)")),
              selectInput(ns("fcol1"),   "Fixed color:", choices = c("default","darkblue","darkred","grey"),selected="default"),
              numericInput(ns("fsize1"), "Fixed size:", value=1, min=1,max=20,step=0.5),
              numericInput(ns("falph1"), "Fixed alpha:", value=1, min=0,max=1,step=.1)
            ),
            column(6,
              selectInput(ns("group1"),  "Group by:", choices = ""),
              selectInput(ns("colour1"), "Colour by:", choices = ""),
              selectInput(ns("shape1"),  "Shape by:", choices = ""),
              selectInput(ns("size1"),   "Size by:", choices = ""),
              selectInput(ns("label1"),  "Label by:", choices = "")
            )
          )
        ),
        # Layer 2
        tabPanel("Layer 2",
		      fluidRow(
		        column(6,
		          selectInput(ns("Xval2"),   "X value:", choices = ""),
		          selectInput(ns("Yval2"),   "Y value:", choices = ""),
	            selectInput(ns("geoms2"),  "Geom:", choices = c("[empty]","point","line","boxplot","bar","histogram","smooth","jitter","text")),
		          selectInput(ns("stats2"),  "Stat:", choices = c("[empty]","mean","median","mean (SD)","median (5-95th perc.)")),
			        selectInput(ns("fcol2"),   "Fixed color:", choices = c("default","darkblue","darkred","grey"),selected="default"),
			        numericInput(ns("fsize2"), "Fixed size:", value=1, min=1,max=20,step=0.5),
			        numericInput(ns("falph2"), "Fixed alpha:", value=1, min=0,max=1,step=.1)
			      ),
			      column(6,
		          selectInput(ns("group2"),  "Group by:", choices = ""),
		          selectInput(ns("colour2"), "Colour by:", choices = ""),
		          selectInput(ns("shape2"),  "Shape by:", choices = ""),
		          selectInput(ns("size2"),   "Size by:", choices = ""),
		          selectInput(ns("label2"),  "Label by:", choices = "")
			      )
		      )
        ),
        # Layer 3
        tabPanel("Layer 3",
		      fluidRow(
		        column(6,
		          selectInput(ns("Xval3"),   "X value:", choices = ""),
		          selectInput(ns("Yval3"),   "Y value:", choices = ""),
	            selectInput(ns("geoms3"),  "Geom:", choices = c("[empty]","point","line","boxplot","bar","histogram","smooth","jitter","text")),
		          selectInput(ns("stats3"),  "Stat:", choices = c("[empty]","mean","median","mean (SD)","median (5-95th perc.)")),
			        selectInput(ns("fcol3"),   "Fixed color:", choices = c("default","darkblue","darkred","grey"),selected="default"),
			        numericInput(ns("fsize3"), "Fixed size:", value=1, min=1,max=20,step=0.5),
			        numericInput(ns("falph3"), "Fixed alpha:", value=1, min=0,max=1,step=.1)
			      ),
			      column(6,
		          selectInput(ns("group3"),  "Group by:", choices = ""),
		          selectInput(ns("colour3"), "Colour by:", choices = ""),
		          selectInput(ns("shape3"),  "Shape by:", choices = ""),
		          selectInput(ns("size3"),   "Size by:", choices = ""),
		          selectInput(ns("label3"),  "Label by:", choices = "")
			      )
		      )
        ),
        # General
        tabPanel("General",
          selectInput(ns("nondups"),  "Non-duplicated:", choices = "",multiple=FALSE),
          textInput(ns("subset"), "Subset", value = ""),
          textInput(ns("precode"), "Pre-code", value = ""),
          textInput(ns("ptitle"), "Title", value = "title"),
          textInput(ns("xlab"), "X label", value = ""),
          textInput(ns("ylab"), "Y label", value = ""),
          numericInput(ns("plheight"), "plot height:", 600),
		      fluidRow(
		        column(6,
              selectInput(ns("facet1"),  "panel by (1):", choices = ""),
			        selectInput(ns("facet2"),  "panel by (2):", choices = ""),
			        selectInput(ns("facet3"),  "panel by (3):", choices = ""),
			        selectInput(ns("facetsc"), "scale panels:", choices = c("fixed","free","free_x","free_y")),
              numericInput(ns("ncol"),   "Number of facet columns:", NA),
			        hr(),
			        checkboxInput(ns("stack"),  "Use stack for barchart", value = TRUE),
		          checkboxInput(ns("Xfact"),  "Set X as factor", value = FALSE),
			        checkboxInput(ns("Yfact"),  "Set Y as factor", value = FALSE),
			        checkboxInput(ns("Ylog"),   "Set Y on log scale", value = FALSE),
			        checkboxInput(ns("Xlog"),   "Set X on log scale", value = FALSE),
			        checkboxInput(ns("omitSE"), "Omit SE from smoother", value = FALSE)
			      ),
			      column(6,
              numericInput(ns("xlim1"), "Lower X limit:", NA),
			        numericInput(ns("xlim2"), "Upper X limit:", NA),
			        numericInput(ns("ylim1"), "Lower Y limit:", NA),
			        numericInput(ns("ylim2"), "Upper Y limit:", NA),
			        hr(),
              numericInput(ns("refint"), "Intercept ref line:", NA),
			        numericInput(ns("refslope"), "Slope ref line:", NA),
			        numericInput(ns("vref"), "Vertical reference line:", NA),
			        hr(),
              #numericInput(ns("npage"), "Number of pages:", 1),
              #checkboxInput(ns("attrl"),"Use attributes", value = FALSE)
			      )
		      )
        )
      ),
      tabBox(width=8,#height="1200px",
        title = "",
        id = ns("exploretabs2"),
        tabPanel("Plot",value="plttab",plotOutput(ns("plotout"),height="600px")),
        tabPanel("Table",value="dttab", div(id="exploretabout",DT::DTOutput(ns("tableout")))),
        tabPanel("Plotly", value="plytab", plotly::plotlyOutput(ns("plotout2"),height = "600px"))
      )
    )
  )
  #actionButton(ns("runscript"), label = "Run Script",icon=icon("code"))
}
#------------------------------------------ module_dataexplore_server ------------------------------------------
#' Data exploration module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_dataexplore_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    # Adapt model list based on selected project location
    observeEvent(r$active_tab,{
      if(r$active_tab=="expl"){
        updateSelectInput(session, "mdls", choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=input$mdls)
      }
    },ignoreInit=TRUE)

    # Select different model (store in reactive values object)
    updfunc <- function(){
      if(input$use_input){
        r$dataIn <- try(readRDS(paste0(r$this_wd,"/shinyMixR/",input$mdls[1],".res.rds"))$origData)
      }else{
        r$dataIn <- try(as.data.frame(readRDS(paste0(r$this_wd,"/shinyMixR/",input$mdls[1],".res.rds"))))
      }
      if(!"try-error" %in% class(r$dataIn)){
        set1 <- paste0(c("Xval","Yval","group","colour","shape","size","label","facet"),rep(1:3,each=8))
        set1 <- lapply(set1,function(x) {
          updateSelectInput(session,x,choices=c("[empty]",names(r$dataIn)),selected=ifelse(input[[x]]=="","[empty]",input[[x]]))
        })
        updateSelectInput(session,"nondups",choices=c("",names(r$dataIn)),selected="")
      }
    }
    observeEvent(input$mdls,{updfunc()},ignoreInit=TRUE)
    observeEvent(input$use_input,{updfunc()},ignoreInit=TRUE)

    # Create actual plot
    observeEvent(input$make, updateTabsetPanel(session, "exploretabs2",selected = "plttab"))
    observeEvent(input$maketbl, updateTabsetPanel(session, "exploretabs2",selected = "dttab"))
    observeEvent(input$makeply, updateTabsetPanel(session, "exploretabs2",selected = "plytab"))
    plheight <- function() return(input$plheight)
    baseexpl <- function(){
      adpr        <- reactiveValuesToList(input)
      adpr$ptitle <- ifelse(adpr$ptitle=="title",adpr$mdls,adpr$ptitle)
      exploreplot(adpr)
    }

    createplot <- eventReactive(input$make,{
      if(!is.null(input$mdls)){
        ptxt <- baseexpl()
        pos  <- regexpr("ggplot(.*)",ptxt)
        eval(parse(text=ptxt))
      }else{
        myalert("Select model/data for analysis",type = "error")
      }
    })#,ignoreInit=TRUE)
    output$plotout   <- renderPlot({print(createplot())},height=plheight,res=100)

    # Create interactive version of plot
    createplot2 <- eventReactive(input$makeply,{
      ptxt <- baseexpl()
      pos  <- regexpr("ggplot(.*)",ptxt)
      ptxt <- paste0(substring(ptxt,1,pos-1),"plotly::ggplotly(\n",substring(ptxt,pos),"\n)")
      if(!is.null(input$mdls)) eval(parse(text=ptxt))
    })
    output$plotout2     <- plotly::renderPlotly(createplot2())

    # Update the dataTable (necessary as different input can be selected, create table when plot is created)
    upDT  <- eventReactive(input$maketbl,{
      if(!is.null(input$mdls)){
        if(!is.null(input$precode) && input$precode!="")     eval(parse(text=input$precode))
        if(!is.null(input$subset)  && input$subset!="")    eval(parse(text=paste0("r$dataIn <- subset(r$dataIn,",input$subset,")")))
        if(!is.null(input$nondups)  && input$nondups!="")  eval(parse(text=paste0("r$dataIn <- subset(r$dataIn, !duplicated(",input$nondups,"))")))
        r$dataIn
      }
    })
    output$tableout <- DT::renderDT(upDT(),options=list(scrollX=TRUE,pageLength=100,lengthMenu=c(10,100,1000,10000)))  # Show entire dataset

    # Clear fields (take into account that if shinyjs is used for updating that the updateSelect no longer works!)
    observeEvent(input$clearfields,{
      fld <- names(input) #[grepl("explore_",names(input))]
      fld <- fld[!fld%in%c("make","maketbl","makeply","save","load","showcode","clearfields","mdls")]
      efld <- paste0(c("Xval","Yval","group","colour","shape","size","label","facet","geoms","stat"),rep(1:3,each=10))
      for(i in setdiff(fld,efld)) shinyjs::reset(i)
      if(!(is.null(input$mdls) || input$mdls=="")){
        efld <- lapply(efld,function(x) updateSelectInput(session,x,selected="[empty]"))
      }else{
        efld <- lapply(efld,function(x) updateSelectInput(session,x,selected=""))
      }
    },ignoreInit=TRUE)
  })
}
