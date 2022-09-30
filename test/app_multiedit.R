

library(shiny)
library(softui)

devtools::load_all()
#library(shintocatman)



jsonMultiEditUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    selectInput(ns("sel_key"), "Categorie", choices = NULL),
    
    tags$label("Keuzelijst"),
    jsonEditModuleUI(ns("edit"))
  )
  
}


jsonMultiEdit <- function(input, output, session, 
                          key = reactive("[]"),
                          value = reactive("[]")){
  
  
  # TODO generic convert from JSON?
  value_txt <- reactive({
    req(value())
    
    out <- jsonlite::fromJSON(value())
    
    if(!is.list(value()) && stringr::str_remove_all(value(), "\"") == ""){
      out <- jsonlite::fromJSON("[]")
    }
    out
    
  })
  
  keys_data <- reactive({
    req(value())
    
    jsonlite::fromJSON(key())
  })
  
  observeEvent(keys_data(), {
    val <- keys_data()
    val <- unname(unlist(val))
    updateSelectInput(session, "sel_key", choices = val)
  })
  
  current_values <- reactive({
    req(input$sel_key)
    jsonlite::toJSON(value_txt()[input$sel_key])
  })
  
  
  callModule(jsonEditModule, "edit", 
             edit = reactive(c("value")), 
             value = current_values) 
  
}




ui <- softui::simple_page(
  softui::box(width=4,
    
    jsonMultiEditUI("test")    
  )
)

keys <- jsonlite::toJSON(list("1" = "Aap", "2" = "Boom", "3" = "Hond"))
vals <- jsonlite::toJSON(list("Aap" = "Chimpansee", "Boom" = "Eik", "Hond" = "Stabij"))


server <- function(input, output, session) {
  
  callModule(jsonMultiEdit, "test", 
             key = reactive(keys),
             value = reactive(vals))
  
  
  
  
}

shinyApp(ui, server)

