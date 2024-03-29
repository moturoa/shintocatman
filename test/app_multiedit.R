

library(shiny)
library(softui)

devtools::load_all()
#library(shintocatman)



jsonMultiEditUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    selectInput(ns("sel_key"), "Categorie", choices = NULL),
    
    tags$label("Keuzelijst"),
    
    uiOutput(ns("ui_listeditor"))
  
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

  
  output$ui_listeditor <- renderUI({
    
    keys <- keys_data()
    
    lapply(names(keys), function(key){
      
      shinyjs::hidden(
        tags$div(id = session$ns(paste0("box_",key)),
                 listEditModuleUI(session$ns(paste0("module_",key)))    
        )  
      )
      
    })
    
  })
  
  observeEvent(input$sel_key, {
    
    key <- input$sel_key
    all_ids <- paste0("box_",names(keys_data()))
    lapply(all_ids, shinyjs::hide)
    this_id <- paste0("box_",key)
    shinyjs::show(this_id)
    
  })

  
  edit_arrays <- reactive({
    
    lapply(names(keys_data()), function(key){
      
      vals <- value_txt()[[key]]
      names(vals) <- as.character(seq_along(vals))
      
      callModule(listEditModule, paste0("module_",key),
                 data = reactive(as.list(vals)),
                 edit_name = TRUE, show_name = TRUE,
                 widths = c(6,6),
                 options = reactive(c("add","delete")),
                 options_labels = c(delete = "Laatste verwijderen", 
                                    add = "Toevoegen"))
      
    })
    
  })
  

  edit_output <- reactive({
    val <- lapply(edit_arrays(), function(x)x())
    names(val) <- names(keys_data())
    val
  })
  
  
return(edit_output)
}




test_jsonMultiEdit <- function(){
  
  devtools::load_all()
  
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
  
}




