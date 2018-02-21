# This widget scripts include functions used by more than one widgets
# In general only functions need for server side are included here for now
# naming of server function is done using camelCase to have the same style as the shiny functions
#------------------------------------------ update_mdlchar ------------------------------------------
# Update all input element in case models are added/deleted
# session: the session object passed to the function given to shinyServer
# metaf: list with the metadata of a model
# pfix: character with the postfix of a widget
updateMdlchar <- function(session,metaf,pfix){
  print(metaf$imp)
  updateSliderInput(session,paste0("imp",pfix),value=metaf$imp)
  updateTextInput(session,paste0("desc",pfix),value=metaf$desc)
  updateSelectInput(session,paste0("ref",pfix),selected=metaf$ref)
  updateTextInput(session,paste0("data",pfix),value=metaf$data)
  updateSelectInput(session,paste0("method",pfix),selected=metaf$method)
}
