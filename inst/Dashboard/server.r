shinyServer(function(input, output, session){

  # Overview - create
  if(all(file.exists(paste0(projwd,c("/analysis","/data","/models","/scripts","/shinyMixR"))))){
    oview <- overview(projloc=projwd)
  }else{
    oview <- data.frame(models="",importance="",description="",ref="",data="",method="",OBJF="",dOBJF=NA,runtime="")
  }
  proxy = DT::dataTableProxy('oviewTable')
  output$oviewTable = DT::renderDataTable(oview,rownames=FALSE,extension=c("Buttons"),filter="bottom",
                                          options=list(scrollX=TRUE,dom="Bfrtip",buttons=c('colvis','csv'),pageLength=25))

  # Overview - create when new folder is selected
  shinyFiles::shinyDirChoose(input, 'projloc', roots=c("Root"=.cwd), session=session)
  observeEvent(input$projloc, selectProjFolder(proj_obj,input,session))

  # Overview - create tree
  tree <- eventReactive(input$createTree,{
    if(file.exists(paste0(projwd,"/shinyMixR"))){ tree_overview(projloc=projwd)}else{data.frame()}
  })
  output$treeOut <- collapsibleTree::renderCollapsibleTree(tree())

  # Overview - other
  observeEvent(input$refrOview,refreshOverview())
  observeEvent(input$adptOview,adaptOverview(proj_obj,input,session,"modal"))
  observeEvent(input$adptOview2,adaptModel(proj_obj,input,session))
  observeEvent(input$delOview,shinyBS::toggleModal(session,"modalDelOview","open"))
  observeEvent(input$delOview2,delOverview(proj_obj,input,session))
  observeEvent(input$modelAdpt,adaptOverview(proj_obj,input,session,"reselect"))

  # Edit model
  observeEvent(input$editLst,updateEditList(proj_obj,input,session))
  observeEvent(input$saveMdl,saveModel(proj_obj,input,session))
  observeEvent(input$saveMdlAs,saveModelAsM(session))
  observeEvent(input$saveMdlAs2,saveModelAs(proj_obj,input,session))
  observeEvent(input$duplMdl, duplModelModal(proj_obj,input,session))
  observeEvent(input$duplMdl2,duplModelSave(proj_obj,input,session))
  observeEvent(input$newMdl, newModelModal(input,session))
  observeEvent(input$newMdl2,newModelSave(proj_obj,input,session))

  # Run a model
  observeEvent(input$runMdl,runMod(proj_obj,input,session,projwd))
  progr <- eventReactive(input$showIt,modProgr(projwd))
  output$progrTxt = renderPrint(progr())

  # Parameter table
  output$parEstTbl = DT::renderDataTable(parTable(input,projloc=projwd),rownames=FALSE,options=list(paging=FALSE,searching=FALSE))
  proxy2 = DT::dataTableProxy('parEstTbl')
  observeEvent(input$parEstLst, DT::replaceData(proxy2, parTable(input,projloc=projwd), rownames = FALSE))
  observeEvent(input$savePars2, parTable(input,session,projloc=projwd,saveit=TRUE))

  # Create GOF plot
  gofpl <- eventReactive(input$gofMdl,gofPlot(input,session,projloc=projwd))
  output$gofPlt   = renderPlot(gofpl())
  observeEvent(input$saveGOF2,gofPlot(input,session,projloc=projwd,saveit=TRUE))

  # Create fit plots
  fitpl <- eventReactive(input$fitMdl,fitPlot(input,projloc=projwd))
  output$fitPlt   = renderPlot(fitpl())
  observeEvent(input$saveFit2,fitPlot(input,session,projloc=projwd,saveit=TRUE))

  # Run R scripts
  observeEvent(input$runScript,createRunScript(input,session,projloc=projwd))
  scrprogr <- eventReactive(input$showScript,showScriptProgress(projloc=projwd))
  output$scriptProgrTxt = renderPrint(scrprogr())

  # Create overall results
  observeEvent(input$refreshRes,refreshResults(input,session,projloc=projwd))
  observeEvent(input$resModLst,udpateResultList(input,session,projloc=projwd))
  observeEvent(input$showAllRes,showResults(input,session,projloc=projwd))
  observeEvent(input$typeRes,changeResults(input,session,projloc=projwd))

  # Settings
  observeEvent(input$fontEditor,shinyAce::updateAceEditor(session, "editor", fontSize=input$fontEditor))
  observeEvent(input$themeEditor,shinyAce::updateAceEditor(session, "editor", theme=input$themeEditor))

  # the test button
  # observeEvent(input$test,{print(c("this is a test",1:3))})

})
