#------------------------------------------ module_edit_ui ------------------------------------------
#' Editor module for UI
#'
#' @description Shiny module for model editor
#'
#' @param id Module id
#' 
#' @export
module_edit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("editLst"),"Model(s)","",multiple=FALSE,selectize = TRUE), #c("",names(projlst)[names(projlst)!="meta"])
    shinyAce::aceEditor(ns("editor"),value="",mode="r",theme = "solarized_light",height="600px",fontSize = 14),
    actionButton(ns("newmdl"), "New Model",icon=icon("file")),
    actionButton(ns("save"), "Save Model",icon=icon("floppy-disk")),
    module_metadata_ui(ns("adapt_meta_ed"),"save"),
    actionButton(ns("updinit"), "Update inits",icon=icon("floppy-disk"))
  )
}
#------------------------------------------ module_edit_server ------------------------------------------
#' Editor module for server
#' 
#' @param id Module id
#' @param r reactive values object that is defined top-level
#' @param settings reactive value with the app settings
#' 
#' @export
module_edit_server <- function(id, r, settings) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(r$active_tab, {
      if(r$active_tab=="editor"){
        updateSelectInput(session, "editLst", choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=input$editLst)
      }
    },ignoreInit=TRUE)

    # Adapt the settings
    observeEvent(settings(),{
      shinyAce::updateAceEditor(session, "editor", fontSize=settings()$fontedt,theme=settings()$themeedt)
    })

    # Update editor when selecting new model
    observeEvent(input$editLst,{
      shinyAce::updateAceEditor(session,"editor",value=paste(readLines(r$proj_obj[[input$editLst]]$model),collapse="\n"))
    },ignoreInit=TRUE)

    # New model
    newmodmodal <- function(){
      ns <- session$ns
      modalDialog(title="New model",easyClose = TRUE,fade=FALSE,
                         textInput(ns("namenew"),"Name new model",value=""),
                         selectInput(ns("templnew"),"template",c("pk.1cmt.closed","pk.1cmt.des")),
                         actionButton(ns("newgo"), "Go",icon=icon("play")))
    }
    observeEvent(input$newmdl,{
      # add code to enter new name and for saving the new model
      toincr <- ifelse(input$editLst=="","run1.r",paste0(input$editLst,".r"))
      updateTextInput(session,"namenew",value=incr_mdl(toincr,"models"))
      showModal(newmodmodal())
    },ignoreInit=TRUE)
    observeEvent(input$newgo,{
        mdl <- try(readLines(system.file(paste0("other/",input$templnew,".r"),package="shinyMixR")))
        if(!"try-error"%in%class(mdl)){
          mdl <- sub("run1",sub("\\.[r|R]","",input$namenew),mdl)
          writeLines(mdl,paste0(r$this_wd,"/models/",input$namenew))
          r$proj_obj <- get_proj(r$this_wd)
          updateSelectInput(session,"editLst",choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=sub("\\.[r|R]","",input$namenew))
          shinyAce::updateAceEditor(session,"editor",value=paste(readLines(paste0(r$this_wd,"/models/",input$namenew)),collapse="\n"))
          removeModal()
        }
    })

    # Save model
    observeEvent(input$save,{
      if(input$editLst!=""){
        writeLines(input$editor,r$proj_obj[[input$editLst]]$model)
        r$proj_obj <- get_proj(r$this_wd)
        myalert("Model saved",type = "success")
      }
    },ignoreInit=TRUE)

    # Handle meta data (we need to pass the selected model as a reactive)
    selectedmodel <- reactive(input$editLst)
    selectedcont  <- reactive(input$editor)
    upd <- module_metadata_server("adapt_meta_ed","save",sellmod=selectedmodel,sellcont=selectedcont,r=r)
    observeEvent(upd(),{
      if(!is.null(upd())){
        updateSelectInput(session,"editLst",choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=sub("\\.[r|R]","",upd()))
        shinyAce::updateAceEditor(session,"editor",value=paste(readLines(r$proj_obj[[sub("\\.[r|R]","",upd())]]$model),collapse="\n"))
        myalert(upd(),type = "success")
      } 
    },ignoreInit=TRUE)

    # Update initial estimates
    initmodal <- function(){
      ns <- session$ns
      if(isTruthy(input$editLst)){
        selm <- tools::file_path_sans_ext(basename(r$proj_obj[[input$editLst]]$model))
        incm <- incr_mdl(basename(r$proj_obj[[input$editLst]]$model),"models")
      }else{
        selm <- incm <- NULL
      } 
      modalDialog(title="Update initial estimates",easyClose = TRUE,size="l",
        selectInput(ns("finest"),"Final estimates from",sub("\\.res\\.rds","",list.files(paste0(r$this_wd,"/shinyMixR"),pattern="res.rds")), selected = selm, multiple=FALSE),
        textInput(ns("tosave"),"Save as",incm),
        actionButton(ns("goupdate"), "Go",icon=icon("play"))
      )
    }
    observeEvent(input$updinit,{showModal(initmodal())},ignoreInit = TRUE)
    observeEvent(input$goupdate,{
      if(isTruthy(input$finest) && isTruthy(input$tosave)){
        shinyWidgets::progressSweetAlert(session=session,id="updateInitProg",title="Updating initial estimates",value = 50)
        res <- try(update_inits(readLines(paste0(r$this_wd,"/models/",input$finest,".r")),
                                paste0(r$this_wd,"/shinyMixR/",input$finest,".res.rds"),
                                paste0(r$this_wd,"/models/",input$tosave)))
        shinyWidgets::closeSweetAlert(session = session)
        if("try-error"%in%class(res)){
          myalert(res,type = "error")
        }else{
          r$proj_obj <- get_proj(r$this_wd)
          updateSelectInput(session,"editLst",choices = names(r$proj_obj)[names(r$proj_obj)!="meta"],selected=sub("\\.[r|R]","",input$tosave))
          shinyAce::updateAceEditor(session,"editor",value=paste(readLines(r$proj_obj[[sub("\\.[r|R]","",input$tosave)]]$model),collapse="\n"))
          myalert("Initials updated",type = "success")
        }
      }else{
        myalert("No model selected to update or no output model defined",type = "error")
      }
      removeModal()
    },ignoreInit = TRUE)
  })
}
