


editDataDefinitionUI <- function(id){
  
  ns <- NS(id)
  
  
  softui::box(
    width = 12,
    title = "WBM Data definition",
    datatableEditModuleUI(ns("data"))  
  )
  
  
  
}




editDataDefinitionModule <- function(input, output, session){
  
  callModule(datatableEditModule, "data", table = "data_definition")
  
}



