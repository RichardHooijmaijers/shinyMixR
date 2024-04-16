#------------------------------------------ module_metadata_ui ------------------------------------------
#' metadata module for UI
#'
#' @description Shiny module for meta data
#'
#' @param id Module id
#' @param type character with the type of button to present (either "save" or "overview")
#' 
#' @export
module_metadata_ui <- function(id,type) {
  ns  <- NS(id)
  lbl <- ifelse(type=="save","Save as","Adapt meta data") 
  icn <- ifelse(type=="save","floppy-disk","list") 
  actionButton(ns("go"), label = lbl, icon=icon(icn))
}

#------------------------------------------ module_metadata_server ------------------------------------------
#' meta data module for server
#' 
#' @param id Module id
#' @param type character with the type of action (either "save" or "overview")
#' @param selline reactive with the selected line for a model (for type "overview")
#' @param sellmod reactive with the selected model (for type "save")
#' @param sellcont reactive with the content of the selected model (for type "save")
#' @param r reactive values object that is defined top-level
#' 
#' @export
module_metadata_server <- function(id,type,selline=NULL,sellmod=NULL,sellcont=NULL,r){
  moduleServer(id, function(input, output, session){
    
    # Function for the modal
    # The modal does not render nicely because of a bug in shiny/DT
    # In case individual column filters are on, inputs in a modal do not render correctly (still active on feb23) 
    # Updating of inputs does not work well in this structure (e.g. when input elements are not present when function is called)
    # But because it is a function we can already fill in the applicable information
    metmodal <- function(){
      ns   <- session$ns
      titl <- ifelse(type=="save","Save as","Adapt model info")
      meta <- list(mdls="",imp=0,ref="",desc="",est="saem",data="",sel="")

      if(!is.null(selline)) meta$sel  <- sort(names(r$proj_obj)[names(r$proj_obj)!="meta"])[selline()]
      if(!is.null(sellmod)) meta$sel  <- sellmod()
      if(length(meta$sel)==0 || meta$sel=="") return()

      meta$imp  <- r$proj_obj[[meta$sel]]$modeleval$meta$imp
      meta$ref  <- r$proj_obj[[meta$sel]]$modeleval$meta$ref
      meta$desc <- r$proj_obj[[meta$sel]]$modeleval$meta$desc
      meta$est  <- r$proj_obj[[meta$sel]]$modeleval$meta$est
      meta$data <- r$proj_obj[[meta$sel]]$modeleval$meta$data

      meta$mdls <- c("",names(r$proj_obj)[names(r$proj_obj)!="meta"])

      gen  <- tagList(
        sliderInput(ns("mdlimp"), "Importance", 0, 4, meta$imp, step = 1, round = TRUE),
        textInput(ns("mdldesc"),"Description",value=meta$desc),
        selectInput(ns("mdlref"),"Reference",meta$ref,choices=tools::file_path_sans_ext(list.files("models")),multiple=FALSE,selectize = TRUE),
        textInput(ns("mdldata"),"Data",value=meta$data),
        selectInput(ns("mdlest"),"Method",c("fo", "foce", "focei", "foi", "nlme", "posthoc", "predict", "rxSolve", "saem", "simulate"),selected=meta$est),
        actionButton(ns("adpt"), "Save",icon=icon("floppy-disk"))    
      )
      mld1 <- tagList(textInput(ns("mdladpt"),"Save as",incr_mdl(paste0(meta$sel,".r"),"models")))
      mld2 <- tagList(selectInput(ns("mdladpt"),"Model",choices=meta$mdls,selected=meta$sel,multiple=FALSE,selectize = TRUE))
      if(type=="save") allt <- tagList(mld1,gen) else allt <- tagList(mld2,gen)
      
      modalDialog(title=titl,easyClose = TRUE,fade=FALSE, allt)  
    }
    meta_ret <- reactiveVal("")

    # First observer to open up the modal
    observeEvent(input$go,{showModal(metmodal())})

    # Second observer to change meta data in case a new model is selected (only in overview)
     observeEvent(input$mdladpt,{
      if(type!="save"){
        if(input$mdladpt!=''){
          meta <- r$proj_obj[[input$mdladpt]]$modeleval$meta
          updateSliderInput(session,"mdlimp",value=meta$imp)
          updateTextInput(session,"mdldesc",value=meta$desc)
          updateSelectInput(session,"mdlref",selected=meta$ref)
          updateTextInput(session,"mdldata",value=meta$data)
          updateSelectInput(session,"mdlest",selected=meta$est)
        }
      }
    },ignoreInit=TRUE)

    # Third observer to save the new meta data to model and project object
    observeEvent(input$adpt,{
      if(input$mdladpt!=''){
        metanfo <- reactiveValuesToList(input)[c("mdlimp","mdldesc","mdlref","mdldata","mdlest")]
        names(metanfo) <- sub("mdl","",names(metanfo))
        # Had to place output of adpt_meta in object otherwise writeLines did not work
        # Added assign projlst so latest changes are saved and are used when multiple changes are made
        # return value is set and can be used in calling module to replace data in overview
        if(type=="save" && !grepl("run[[:digit:]]*\\.[r|R]",input$mdladpt)){
          myalert("model could not be saved, please make sure names is defined as 'run[digit]'",type="error")
          return()
        }
        if(type=="save"){
          tmpmod <- tempfile()
          writeLines(sellcont(),tmpmod)
          #toret  <- c(name=paste0("models/",sellmod(),".r"), val=input$mdladpt, saveas=paste0("models/",input$mdladpt))
          toret  <- c(name=tmpmod, val=input$mdladpt, saveas=paste0("models/",input$mdladpt))
        }else{
          toret <- c(name=paste0("models/",input$mdladpt,".r"), val="Update DT", saveas=paste0("models/",input$mdladpt,".r"))
        } 
        towr <- adpt_meta(toret['name'],metanfo)
        if(type=="save") towr <- sub(sellmod(),sub("\\.[r|R]","",input$mdladpt),towr)
        writeLines(towr,toret['saveas'])
        r$proj_obj <- get_proj(r$this_wd)
        removeModal()
        meta_ret(toret['val'])
      }
    },ignoreInit = TRUE)
    # Return value what type of change took place
    return(reactive(meta_ret()))
  })
}  

