# The widget script should at least contain the following:
# 1. a list named 'info' providing information about the widget (with at least name title and icon)
# 2. function named 'widgetui' containing the ui inputs as taglist
# 3. function named 'widgetserver' conatining the server logic
info <- list(name="editor",
             title="Edit model(s)",
             icon="edit")

widgetui <- function() {
 tagList(
   selectInput("editor_editLst","Model(s)","",multiple=FALSE,selectize = TRUE), #c("",names(projlst)[names(projlst)!="meta"])
   shinyAce::aceEditor("editor_editor",value="",mode="r",theme = "solarized_light",height="600px",fontSize = 14),
   actionButton("editor_newmdl", "New Model",icon=icon("file-text-o")),
   actionButton("editor_save", "Save Model",icon=icon("save")),
   actionButton("editor_saveas", "Save Model as",icon=icon("save"))
  )
}
widgetserver <- function(input,output,session){
  # Adapt model list based on selected project location
  observeEvent(input$tabs,{
    if(input$tabs=="editor"){
      updateSelectInput(session, "editor_editLst", choices = names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"],selected=input$editor_editLst)
    }
  },ignoreInit=TRUE)

  # Update editor when selecting new model
  observeEvent(input$editor_editLst,{
    shinyAce::updateAceEditor(session,"editor_editor",value=paste(readLines(proj_obj[[input$editor_editLst]]$model),collapse="\n"))
  },ignoreInit=TRUE)

  # New model
  editornew = modalDialog(title="New model",easyClose = TRUE,fade=FALSE,
                         textInput("editor_namenew","Name new model",value=""),
                         selectInput("editor_templnew","template",c("pk.1cmt.closed","pk.1cmt.des")),
                         actionButton("editor_newgo", "Go",icon=icon("play")))
  observeEvent(input$editor_newmdl,{
    # add code to enter new name and for saving the new model
    toincr <- ifelse(input$editor_editLst=="","run1.r",paste0(input$editor_editLst,".r"))
    updateTextInput(session,"editor_namenew",value=incr_mdl(toincr,"models"))
    showModal(editornew)
  },ignoreInit=TRUE)
  observeEvent(input$editor_newgo,{
      mdl <- try(readLines(system.file(paste0("Other/",input$editor_templnew,".r"),package="shinyMixR")))
      if(class(mdl)!="try-error"){
        mdl <- sub("run1",sub("\\.[r|R]","",input$editor_namenew),mdl)
        writeLines(mdl,paste0("models/",input$editor_namenew))
        assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
        updateSelectInput(session,"editor_editLst",choices = names(get("proj_obj",pos = .GlobalEnv)),selected=sub("\\.[r|R]","",input$editor_namenew))
        shinyAce::updateAceEditor(session,"editor_editor",value=paste(readLines(proj_obj[[input$editor_editLst]]$model),collapse="\n"))
        removeModal()
      }
  })

  # Save model
  observeEvent(input$editor_save,{
    if(input$editor_editLst!=""){
      writeLines(input$editor_editor,proj_obj[[input$editor_editLst]]$model)
      assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
      shinyWidgets::sendSweetAlert(session = session,text = "Model saved",type = "success")
    }
  },ignoreInit=TRUE)

  # Save model as - omitted duplicate model as this is basically the same as save as
  editorsaveas = modalDialog(title="Save as",easyClose = TRUE,fade=FALSE,
                             textInput("editor_saveasname", "Save as", value = ""),
                             sliderInput("editor_impsaveas", "Importance", 0, 5, 0, step = 1, round = TRUE),
                             textInput("editor_descsaveas","Description",value=""),
                             selectInput("editor_refsaveas","Reference",c("",names(get("proj_obj",pos = .GlobalEnv))[names(get("proj_obj",pos = .GlobalEnv))!="meta"]),multiple=FALSE,selectize = TRUE),
                             textInput("editor_datasaveas","Data",value=""),
                             selectInput("editor_estsaveas","method",c("saem","nlme","nlme.mu","nlme.mu.cov","nlme.free")),
                             actionButton("editor_saveas2","Save",icon=icon("save")))
  observeEvent(input$editor_saveas, {
    if(length(proj_obj)==0) assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
    newmeta <- proj_obj[[tools::file_path_sans_ext(input$editor_editLst)]]$modeleval$meta
    updateTextInput(session,"editor_saveasname",value=incr_mdl(paste0(input$editor_editLst,".r"),"models"))
    updateSliderInput(session,"editor_impsaveas",value=newmeta$imp)
    updateTextInput(session,"editor_descsaveas",value=newmeta$desc)
    updateSelectInput(session,"editor_refsaveas",selected=newmeta$ref)
    updateTextInput(session,"editor_datasaveas",value=newmeta$data)
    updateSelectInput(session,"editor_estsaveas",selected=newmeta$est)
    showModal(editorsaveas)
  },ignoreInit=TRUE)
  
  saveasfunc <- function(){
    metanfo <- reactiveValuesToList(input)[c("editor_impsaveas","editor_descsaveas","editor_refsaveas","editor_datasaveas","editor_estsaveas")]
    names(metanfo) <- gsub("editor_|saveas","",names(metanfo))
    towr <- adpt_meta(paste0("models/",input$editor_editLst,".r"),metanfo)
    towr <- sub(input$editor_editLst,sub("\\.[r|R]","",input$editor_saveasname),towr)
    writeLines(towr,paste0("models/",input$editor_saveasname))
    assign("proj_obj",get_proj(),pos = .GlobalEnv,inherits=TRUE)
    updateSelectInput(session,"editor_editLst",choices = names(get("proj_obj",pos = .GlobalEnv)),selected=sub("\\.[r|R]","",input$editor_saveasname))
    removeModal()
    Sys.sleep(0.4) # Apparently need some time to close the modal before alert can be shown (is also modal!)
    shinyWidgets::sendSweetAlert(session = session,title="Save",text = paste0("Model saved as ",input$editor_saveasname),type = "success")
  }
  observeEvent(input$editor_saveas2,{
    if(file.exists(paste0("models/",input$editor_saveasname))){
      shinyWidgets::confirmSweetAlert(session=session,title="Save as",inputId="editor_confsaveas",text="File exists, do you want to overwrite it?")
    }else{
      saveasfunc()
    }
  },ignoreInit=TRUE)
  observeEvent(input$editor_confsaveas,{
    if(isTRUE(input$editor_confsaveas)) saveasfunc()
  })
}
