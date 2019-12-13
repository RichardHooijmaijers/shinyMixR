# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="overview",
             title="Model overview",
             icon="folder")

widgetui <- function() {
  tagList(
    tags$head(tags$style(HTML(".shiny-notification{position:fixed;top: calc(50%);left: calc(50%);}"))),
    actionButton("overview_refr", "Refresh",icon=icon("refresh")),
    actionButton("overview_adpt", "Adapt model notes",icon=icon("list")),
    actionButton("overview_del", "Delete model(s)",icon=icon("remove")),
    br(),br(),
    shinydashboard::box(width=NULL,title = span(id="projTitle1",span(id="projTitle2","Overview")), solidHeader = TRUE, status = "primary",collapsible = TRUE,
      DT::dataTableOutput('overview_tbl')
    ),
    shinydashboard::box(width=NULL,title = "Tree View",solidHeader = TRUE, status = "primary",collapsible = TRUE,collapsed = TRUE,
      actionButton("overview_mktree", "make tree",icon=icon("tree")),
      collapsibleTree::collapsibleTreeOutput("overview_treeout")
    )
   )
}

widgetserver <- function(input,output,session){
  #cat("time now is",difftime(Sys.time(),strt,units="secs"),"\n")
  # Create overview when app is loaded
  if(all(file.exists(c("analysis","data","models","scripts","shinyMixR")))){
    overview_ov <- overview()
    #cat("files exist\n")
  }else{
    #cat("files not exist\n")
    overview_ov <- data.frame(models="",importance="",description="",ref="",data="",method="",OBJF="",dOBJF=NA,runtime="")
  }
  proxy = DT::dataTableProxy('overview_tbl')
  output$overview_tbl = DT::renderDataTable(overview_ov,rownames=FALSE,extension=c("Buttons"),filter="bottom",
                                            options=list(scrollX=TRUE,dom="Bfrtip",buttons=c('colvis','csv'),pageLength=25))
  # Create tree
  tree <- eventReactive(input$overview_mktree,{
    if(file.exists("shinyMixR")){tree_overview()}else{data.frame()}
  })
  output$overview_treeout <- collapsibleTree::renderCollapsibleTree(tree())

  # Refresh overview
  observeEvent(input$overview_refr,{
    if(file.exists("shinyMixR")){
      assign("proj_obj",get_proj(),pos = .GlobalEnv)
      overview_ov <- overview()
      DT::replaceData(proxy, overview_ov, rownames = FALSE)
      ##### DOUBLE CHECK - UPDATING OF INFO SHOULD BE DONE IN APPLICABLE WIDGETS - LIKELY USING input$tabs TO CHECK..
      #updateSelectInput(session,"editLst",choices=c("",names(projlst)[names(projlst)!="meta"]))
      #for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=names(projlst)[names(projlst)!="meta"])
      #updateSelectInput(session,"scriptFilLst",choices=list.files(paste0(projwd,"/scripts")))
      #updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projwd,"/analysis"),recursive=FALSE,full.names=FALSE))
    }
  },ignoreInit = TRUE)

  # Create modal for meta data
  oviewmodal = modalDialog(title="Adapt model info",easyClose = TRUE,
                           selectInput("overview_mdladpt","Model","",multiple=FALSE,selectize = TRUE),
                           sliderInput("overview_mdlimp", "Importance", 0, 4, 0, step = 1, round = TRUE),
                           textInput("overview_mdldesc","Description",value=""),
                           selectInput("overview_mdlref","Reference","",multiple=FALSE,selectize = TRUE),
                           textInput("overview_mdldata","Data",value=""),
                           selectInput("overview_mdlest","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
                           actionButton("overview_adpt2", "Go",icon=icon("play")))

  # fill in meta data based on selected model
  observeEvent(input$overview_adpt,{
    if(length(proj_obj)==0) return()
    if(!is.null(input$overview_tbl_rows_selected)){
      msel <- names(proj_obj)[names(proj_obj)!="meta"][input$overview_tbl_rows_selected]
      meta <- proj_obj[[msel]]$modeleval$meta
    }else{
      msel <- ''
      meta <- list(imp=NA,ref='',desc='',est='saem',data='')
    }
    # This list was previously within updateMdlchar in widget_misc.r --> CHECK IF MAKE FUNCTION AGAIN (OCCURS QUITE SOME TIMES)
    updateSelectInput(session,"overview_mdladpt",choices=names(proj_obj)[names(proj_obj)!="meta"],selected=msel)
    updateSliderInput(session,"overview_mdlimp",value=meta$imp)
    updateTextInput(session,"overview_mdldesc",value=meta$desc)
    updateSelectInput(session,"overview_mdlref",choices=names(proj_obj)[names(proj_obj)!="meta"],selected=meta$ref)
    updateTextInput(session,"overview_mdldata",value=meta$data)
    updateSelectInput(session,"overview_mdlest",selected=meta$est)
    showModal(oviewmodal)
  })

  # change meta data in case different model is selected within modal
  observeEvent(input$overview_mdladpt,{
    if(input$overview_mdladpt!=''){
      meta <- proj_obj[[input$overview_mdladpt]]$modeleval$meta
      updateSliderInput(session,"overview_mdlimp",value=meta$imp)
      updateTextInput(session,"overview_mdldesc",value=meta$desc)
      updateSelectInput(session,"overview_mdlref",selected=meta$ref)
      updateTextInput(session,"overview_mdldata",value=meta$data)
      updateSelectInput(session,"overview_mdlest",selected=meta$est)
    }
  })

  # Save the new meta data to model and project object
  observeEvent(input$overview_adpt2,{
    if(input$overview_mdladpt!=''){
      metanfo <- reactiveValuesToList(input)[c("overview_mdlimp","overview_mdldesc","overview_mdlref","overview_mdldata","overview_mdlest")]
      names(metanfo) <- sub("overview_mdl","",names(metanfo))
      # names(metanfo) <- c("imp","desc","ref","data","")
      # Had to place output of adpt_meta in object otherwise writeLines did not work
      # Added assign projlst so latest changes are saved and are used when multiple changes are made
      towr <- adpt_meta(paste0("models/",input$overview_mdladpt,".r"),metanfo)
      writeLines(towr,paste0("models/",input$overview_mdladpt,".r"))
      assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
      DT::replaceData(proxy, overview(), rownames = FALSE)
      removeModal()
    }
  },ignoreInit = TRUE)

  # Delete models
  delmodmodal = modalDialog(title="Delete model(s)",easyClose = TRUE,
                            checkboxInput("overview_delmodall","Delete all models and results",value=TRUE),
                            actionButton("overview_del2", "Go",icon=icon("play")))
  observeEvent(input$overview_del,{showModal(delmodmodal)},ignoreInit = TRUE)
  observeEvent(input$overview_del2,{
    if(!is.null(input$overview_tbl_rows_selected)){
      msel <- names(proj_obj)[names(proj_obj)!="meta"][input$overview_tbl_rows_selected]
      if(input$overview_delmodall) {
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
  #cat("time now is",difftime(Sys.time(),strt,units="secs"),"\n")
}

#cat("says rows selected'",input$overview_tbl_rows_selected,"'\n")
#print(str(input$overview_tbl_rows_selected))
#msel <- names(proj_obj)[names(proj_obj)!="meta"][input$overview_tbl_rows_selected]

    # msel <- ifelse(type=="modal",names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected],inp$modelAdpt)
    # if(length(msel)!=0 && msel!="" && !is.na(msel)) eval(parse(text=body(eval(parse(text=readLines(projlst[[msel]]$model))))[4]))
    # Only works if get_proj has geteval=TRUE which is the default for the shinyMixR interface
    #meta <- proj_obj[[msel]]$modeleval$meta

      #print(meta)
#updateSelectInput(session,"overview_mdladpt",choices=names(proj_obj)[names(proj_obj)!="meta"],selected=msel)

    #updateSelectInput(session,"overview_mdlimp",choices=)
    #updateSelectInput(session,"refAdpt",choices=c("",names(projlst)[names(projlst)!="meta"]))
      #updateMdlchar(session,meta,"Adpt")
      #print(names(projlst)[names(projlst)!="meta"])
      #updateSelectInput(session,"modelAdpt",selected=msel,choices=names(projlst)[names(projlst)!="meta"])
      #if(type=="modal") shinyBS::toggleModal(session,"modalAdptOview","open")



# #------------------------------------------ adaptOverview ------------------------------------------
# #' @export
# # Adapt the model overview with the meta data for the selected model
# # type: character. In case set to "modal" it will take the model name from the overview otherwise from the input element
# adaptOverview <- function(projlst,inp,session,type="modal"){
#   if(length(projlst)==0) return()
#   msel <- ifelse(type=="modal",names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected],inp$modelAdpt)
#   # if(length(msel)!=0 && msel!="" && !is.na(msel)) eval(parse(text=body(eval(parse(text=readLines(projlst[[msel]]$model))))[4]))
#   # Only works if get_proj has geteval=TRUE which is the default for the shinyMixR interface
#   meta <- projlst[[msel]]$modeleval$meta
#   #print(meta)
#   updateSelectInput(session,"refAdpt",choices=c("",names(projlst)[names(projlst)!="meta"]))
#   updateMdlchar(session,meta,"Adpt")
#   #print(names(projlst)[names(projlst)!="meta"])
#   updateSelectInput(session,"modelAdpt",selected=msel,choices=names(projlst)[names(projlst)!="meta"])
#   if(type=="modal") shinyBS::toggleModal(session,"modalAdptOview","open")
# }
# #------------------------------------------ AdaptModel ------------------------------------------
# #' @export
# # Adapt and write the model based on the new meta data changed by the user
# adaptModel <- function(projlst,inp,session){
#   if(inp$modelAdpt!=""){
#     metanfo <- reactiveValuesToList(inp)[c("impAdpt","descAdpt","refAdpt","dataAdpt","estAdpt")]
#     names(metanfo) <- sub("Adpt","",names(metanfo))
#     # Had to place output of adpt_meta in object otherwise writeLines did not work
#     # Added assign projlst so latest changes are saved and are used when multiple changes are made
#     towr <- adpt_meta(paste0(projwd,"/models/",inp$modelAdpt,".r"),metanfo)
#     writeLines(towr,paste0(projwd,"/models/",inp$modelAdpt,".r"))
#     assign(deparse(substitute(projlst)),get_proj(projwd),pos = .GlobalEnv,inherits=TRUE)
#     shinyBS::toggleModal(session,"modalAdptOview","close")
#     proxy = DT::dataTableProxy('oviewTable')
#     # Could not use ColReorder extension in combination with replaceData!
#     DT::replaceData(proxy, overview(projloc=projwd), rownames = FALSE)
#   }
# }
# #------------------------------------------ delOverview ------------------------------------------
# #' @export
# # delete the the selected model(s)
# delOverview <- function(projlst,inp,session){
#   msel <- names(projlst)[names(projlst)!="meta"][inp$oviewTable_rows_selected]
#   if(inp$delAllMod) {
#     try(file.remove(paste0(projwd,"/shinyMixR/",msel,".res.rds")))
#     try(file.remove(paste0(projwd,"/shinyMixR/",msel,".ressum.rds")))
#     try(unlink(paste0(projwd,"/analysis/",msel),recursive = TRUE))
#   }
#   try(file.remove(paste0(projwd,"/models/",msel,".r")))
#   assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
#   modnm <- names(get(deparse(substitute(projlst)),pos = .GlobalEnv))
#   updateSelectInput(session,"editLst",choices=c("",modnm[modnm!="meta"]))
#   for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=modnm[modnm!="meta"])
#   shinyBS::toggleModal(session,"modalDelOview","close")
# }
# #------------------------------------------ selectProjFolder ------------------------------------------
# #' @export
# # select different project folder
# selectProjFolder <- function(projlst,inp,session){
#   if(class(inp$projloc)[1]=="list"){
#     projwd <- ifelse(length(shinyFiles::parseDirPath(c("Root"=.cwd), inp$projloc))==0,".",shinyFiles::parseDirPath(c("Root"=.cwd), inp$projloc))
#     projwd <- normalizePath(projwd,winslash="/")
#     create_proj(projwd)
#     removeUI(selector = "#projTitle2")
#     insertUI(selector = "#projTitle1", ui=HTML(paste("<span id='projTitle2'>Overview - ",sub(normalizePath(.cwd,winslash="/"),".",projwd),"</span>")))
#     assign("projwd",projwd,pos = .GlobalEnv)
#     assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv,inherits=TRUE)
#     updateSelectInput(session,"editLst",choices=c("",names(projlst)[names(projlst)!="meta"]))
#     for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=names(projlst)[names(projlst)!="meta"])
#     updateSelectInput(session,"scriptFilLst",choices=list.files(paste0(projwd,"/scripts")))
#     updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projwd,"/analysis"),recursive=FALSE,full.names=FALSE))
#     if(file.exists(paste0(projwd,"/shinyMixR"))){
#       oview <- overview(projloc=projwd)
#       proxy = DT::dataTableProxy('oviewTable')
#       DT::replaceData(proxy, oview, rownames = FALSE)
#     }
#   }
# }
# #------------------------------------------ refreshOverview ------------------------------------------
# #' @export
# # refresh overview
# refreshOverview <- function(projlst,session){
#   if(file.exists(paste0(projwd,"/shinyMixR"))){
#     assign(deparse(substitute(projlst)),get_proj(projloc=projwd),pos = .GlobalEnv)
#     oview <- overview(projloc=projwd)
#     proxy = DT::dataTableProxy('oviewTable')
#     DT::replaceData(proxy, oview, rownames = FALSE)
#     updateSelectInput(session,"editLst",choices=c("",names(projlst)[names(projlst)!="meta"]))
#     for(i in c("runLst","gofLst","fitLst","parEstLst","scriptModLst")) updateSelectInput(session,i,choices=names(projlst)[names(projlst)!="meta"])
#     updateSelectInput(session,"scriptFilLst",choices=list.files(paste0(projwd,"/scripts")))
#     updateSelectInput(session,"resModLst",choices=list.dirs(paste0(projwd,"/analysis"),recursive=FALSE,full.names=FALSE))
#   }
# }


# shinyBS::bsModal("overview_modalapdt","Adapt model notes","",
#   selectInput("modelAdpt","Model","",multiple=FALSE,selectize = TRUE), #c("",names(projlst)[names(projlst)!="meta"])
#   sliderInput("impAdpt", "Importance", 0, 5, 0, step = 1, round = TRUE),
#   textInput("descAdpt","Description",value=""),
#   selectInput("refAdpt","Reference","",multiple=FALSE,selectize = TRUE), # c("",names(projlst)[names(projlst)!="meta"])
#   textInput("dataAdpt","Data",value=""),
#   selectInput("estAdpt","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
#   actionButton("adptOview2", "Go",icon=icon("play"))
# ),

# shinyBS::bsModal("modalDelOview","Delete model(s)","",
#   checkboxInput("delAllMod","Delete all models and results",value=TRUE),
#   actionButton("delOview2", "Go",icon=icon("play"))
# ),
# observeEvent(input$adptOview2,adaptModel(proj_obj,input,session))
# observeEvent(input$delOview,shinyBS::toggleModal(session,"modalDelOview","open"))
# observeEvent(input$delOview2,delOverview(proj_obj,input,session))
# observeEvent(input$modelAdpt,adaptOverview(proj_obj,input,session,"reselect"))
