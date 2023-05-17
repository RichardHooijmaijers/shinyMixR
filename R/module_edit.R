#------------------------------------------ module_edit_ui ------------------------------------------
#' Editor module for UI
#'
#' @description Shiny module for model editor
#'
#' @param id,input,output,session Internal parameters for {shiny}
#' @export
#' @noRd
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
#' @param tabswitch reactive value that monitors the tabswitch
#' @param settings reactive value with the app settings
#' @noRd 
#' @export
module_edit_server <- function(id,tabswitch,settings) {
  moduleServer(id, function(input, output, session) {
    # Adapt model list based on selected project location
    observeEvent(tabswitch(),{
      if(tabswitch()=="editor"){
        updateSelectInput(session, "editLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$editLst)
      }
    },ignoreInit=TRUE)

    # Adapt the settings
    observeEvent(settings(),{
      shinyAce::updateAceEditor(session, "editor", fontSize=settings()$fontedt,theme=settings()$themeedt)
    })

    # Update editor when selecting new model
    observeEvent(input$editLst,{
      shinyAce::updateAceEditor(session,"editor",value=paste(readLines(proj_obj[[input$editLst]]$model),collapse="\n"))
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
        mdl <- try(readLines(system.file(paste0("Other/",input$templnew,".r"),package="shinyMixR")))
        if(!"try-error"%in%class(mdl)){
          mdl <- sub("run1",sub("\\.[r|R]","",input$namenew),mdl)
          writeLines(mdl,paste0("models/",input$namenew))
          assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
          updateSelectInput(session,"editLst",choices = names(get("proj_obj",pos = .GlobalEnv)),selected=sub("\\.[r|R]","",input$namenew))
          shinyAce::updateAceEditor(session,"editor",value=paste(readLines(paste0("models/",input$namenew)),collapse="\n"))
          removeModal()
        }
    })

    # Save model
    observeEvent(input$save,{
      if(input$editLst!=""){
        writeLines(input$editor,proj_obj[[input$editLst]]$model)
        assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
        myalert("Model saved",type = "success")
        # Do not really like the alerts from bs4dash so stick to shinywdigets
        # createAlert(id = NULL,selector = NULL,options=list(title = "Alert",closable = TRUE,width = 12,elevations = 1,status = "primary",content = "Model saved"))
      }
    },ignoreInit=TRUE)

    # Handle meta data (we need to pass the selected model as a reactive)
    selectedmodel <- reactive(input$editLst)
    upd <- module_metadata_server("adapt_meta_ed","save",sellmod=selectedmodel)
    observeEvent(upd(),{
      if(!is.null(upd())){
        updateSelectInput(session,"editLst",choices = names(get("proj_obj",pos = .GlobalEnv)),selected=sub("\\.[r|R]","",upd()))
        shinyAce::updateAceEditor(session,"editor",value=paste(readLines(proj_obj[[sub("\\.[r|R]","",upd())]]$model),collapse="\n"))
        myalert(upd(),type = "success")
      } 
    },ignoreInit=TRUE)

    # Update initial estimates
    initmodal <- function(){
      ns <- session$ns
      modalDialog(title="Update initial estimates",easyClose = TRUE,size="l",
        selectInput(ns("finest"),"Final estimates from",sub("\\.res\\.rds","",list.files("shinyMixR",pattern="res.rds")),
                    selected=tools::file_path_sans_ext(basename(proj_obj[[input$editLst]]$model)),multiple=FALSE),
        textInput(ns("tosave"),"Save as",incr_mdl(basename(proj_obj[[input$editLst]]$model),"models")),
        actionButton(ns("goupdate"), "Go",icon=icon("play"))
      )
    }
    observeEvent(input$updinit,{showModal(initmodal())},ignoreInit = TRUE)
    observeEvent(input$goupdate,{
      if(input$editor!=""){
        res <- try(update_inits(input$editor,paste0("shinyMixr/",input$finest,".res.rds"),paste0("models/",input$tosave)))
        if("try-error"%in%class(res)){
          myalert("Could not update initials",type = "error")
        }else{
          assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
          updateSelectInput(session,"editLst",choices = names(get("proj_obj",pos = .GlobalEnv)),selected=sub("\\.[r|R]","",input$tosave))
          shinyAce::updateAceEditor(session,"editor",value=paste(readLines(proj_obj[[sub("\\.[r|R]","",input$tosave)]]$model),collapse="\n"))
          myalert("Initials updated",type = "success")
        }
      }else{
        myalert("No model selected to update",type = "error")
      }
      removeModal()
    },ignoreInit = TRUE)
  })
}
