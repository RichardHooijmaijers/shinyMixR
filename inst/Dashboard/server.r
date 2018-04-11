shinyServer(function(input, output, session){

  # Overview (could not use ColReorder extension in combination with replaceData)
  # Somehow the DT:renderDataTable throws a warning (while overview itself not)?
  output$oviewTable = DT::renderDataTable(overview(),rownames=FALSE,extension=c("Buttons"),filter="bottom",
                                          options=list(scrollX=TRUE,dom="Bfrtip",buttons=c('colvis','csv'),pageLength=25))
  proxy = DT::dataTableProxy('oviewTable')
  observeEvent(input$refrOview, DT::replaceData(proxy, overview(), rownames = FALSE))
  tree <- eventReactive(input$createTree,tree_overview())
  output$treeOut <- collapsibleTree::renderCollapsibleTree(tree())

  observeEvent(input$adptOview,adaptOverview(proj_obj,input,session,"modal"))
  observeEvent(input$modelAdpt,adaptOverview(proj_obj,input,session,"reselect"))
  observeEvent(input$adptOview2,adaptModel(proj_obj,input,session))

  # Edit model
  observeEvent(input$editLst,updateEditList(proj_obj,input,session))
  observeEvent(input$saveMdl,saveModel(proj_obj,input,session))
  observeEvent(input$duplMdl, duplModelModal(proj_obj,input,session))
  observeEvent(input$duplMdl2,duplModelSave(proj_obj,input,session))
  observeEvent(input$newMdl, newModelModal(input,session))
  observeEvent(input$newMdl2,newModelSave(proj_obj,input,session))

  # Run a model
  observeEvent(input$runMdl,runMod(proj_obj,input,session))
  progr <- eventReactive(input$showIt,modProgr())
  output$progrTxt = renderPrint(progr())

  # Parameter table
  output$parEstTbl = DT::renderDataTable(parTable(input),rownames=FALSE,options=list(paging=FALSE,searching=FALSE))
  proxy2 = DT::dataTableProxy('parEstTbl')
  observeEvent(input$parEstLst, DT::replaceData(proxy2, parTable(input), rownames = FALSE))
  observeEvent(input$savePars2, parTable(input,session,TRUE))

  # Create GOF plot
  gofpl <- eventReactive(input$gofMdl,gofPlot(input,session))
  output$gofPlt   = renderPlot(gofpl())
  observeEvent(input$saveGOF2,gofPlot(input,session,TRUE))

  # Create fit plots
  fitpl <- eventReactive(input$fitMdl,fitPlot(input))
  output$fitPlt   = renderPlot(fitpl())
  observeEvent(input$saveFit2,fitPlot(input,session,TRUE))

  # Create overall results
  observeEvent(input$refreshRes,refreshResults(input,session))
  observeEvent(input$resModLst,udpateResultList(input,session))
  observeEvent(input$showAllRes,showResults(input,session))
  observeEvent(input$typeRes,changeResults(input,session))

  # Run R scripts
  observeEvent(input$runScript,createRunScript(input,session))
  scrprogr <- eventReactive(input$showScript,showScriptProgress())
  output$scriptProgrTxt = renderPrint(scrprogr())

  # Settings
  observeEvent(input$fontEditor,shinyAce::updateAceEditor(session, "editor", fontSize=input$fontEditor))
  observeEvent(input$themeEditor,shinyAce::updateAceEditor(session, "editor", theme=input$themeEditor))

  # the test button
  # observeEvent(input$test,{print(c("this is a test",1:3))})

})
