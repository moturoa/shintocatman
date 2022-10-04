

library(shiny)
library(softui)

devtools::load_all()
#library(shintocatman)



jsonMultiEditUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    selectInput(ns("sel_key"), "Categorie", choices = NULL),
    
    tags$label("Keuzelijst"),
    
    listEditModuleUI(ns("edit"))
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
    
    nms <- names(val)
    val <- unname(unlist(val))
    val <- setNames(nms,val)
    updateSelectInput(session, "sel_key", choices = val)
  })
  
  current_values <- reactive({
    req(input$sel_key)
    vals <- value_txt()[[input$sel_key]]
    names(vals) <- as.character(seq_along(vals))
    as.list(vals)
  })
  
  
  callModule(listEditModule, "edit",
             data = current_values,
             edit_name = TRUE, show_name = TRUE,
             widths = c(6,6),
             options = reactive(c("add","delete")),
             options_labels = c(delete = "Laatste verwijderen", 
                                add = "Toevoegen"))
  

  
}




ui <- softui::simple_page(
  softui::box(width=4,
    
    jsonMultiEditUI("test")    
  ),
  softui::box(width=4,
              
        verbatimTextOutput("txt_out")
  )
)

keys <- jsonlite::toJSON(list("1" = "Aap", "2" = "Boom", "3" = "Hond"))
vals <- jsonlite::toJSON(list("1" = c("Chimpansee", "Gorilla"), 
                              "2" = c("Eik", "Beuk"), 
                              "3" = "Stabij"))


server <- function(input, output, session) {
  
  out <- callModule(jsonMultiEdit, "test", 
             key = reactive(keys),
             value = reactive(vals))
  
  
  output$txt_out <- renderPrint({
    out()
  })
  
  
}

shinyApp(ui, server)

