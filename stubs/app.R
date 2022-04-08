
# edit 1 cell van json vector
jsonValueEditUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
    column(12, 
           uiOutput(ns("ui_value_edit"))       
           )
    
  )
  
}

jsonValueEditModule <- function(input, output, session, 
                                type = NULL,
                                value = NULL){
  

  output$ui_value_edit <- renderUI({
    
    req(type)
    
    if(type == "text"){
      textInput(session$ns("input_value"), NULL, 
                value = as.character(value),
                width  = "100%")
    } else if(type == "integer"){
      numericInput(session$ns("input_value"), NULL, 
                   step = 1, 
                   value = as.integer(value),
                   width  = "100%")
    } else if(type == "color"){
      colourpicker::colourInput(session$ns("input_value"), NULL, 
                                value = as.character(value))
    } 
    
  })
    
  
  return(
    reactive(input$value)
  )
  
}


jsonEditVectorUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    uiOutput(ns("ui_extra_input")),
    uiOutput(ns("ui_edit_values")),
    uiOutput(ns("ui_add_button")),
    tags$hr(),
    verbatimTextOutput(ns("txt_out"))
  )
}


jsonEditVectorModule <- function(input, output, session, 
                                 type = reactive(NULL),
                                 data = reactive(NULL),
                                 column = reactive(NULL)){
  
  
  value <- reactive({
    req(data())
    data()[[column()]]
  })
  
  n_items <- reactiveVal()
  observeEvent(value(), n_items(length(value())))
  
  observeEvent(input$btn_add_value, {
    
     n_items(n_items() + 1)
    
  })
  
  
  output$ui_extra_input <- renderUI({
    req(data())
    type <- type()
    req(type)
    
    
    if(type == "order"){
      
      selectInput(session$ns("sel_column_order"), "Order which column:",
                  choices = names(data()), selected = character(0))
    }
    
    
  })
  
  output$ui_add_button <- renderUI({
    
    req(input$sel_type)
    req(data())
    
    if(input$sel_type != "order"){
      
      actionButton(session$ns("btn_add_value"), "Voeg toe",icon = icon("plus"))    
      
    }
    
  })
  
  
  labels_to_order_by <- reactive({
    ord <- input$sel_column_order
    req(ord)
    data()[[ord]]
  })
  
  value_ids <- reactiveVal()
  
  output$ui_edit_values <- renderUI({
    
    type <- type()
    req(type)
    
    if(type == "order"){
      
      ord <- input$sel_column_order
      req(ord)
      labels <- labels_to_order_by()
        
      rank_list(
        text = "Change the order",
        labels = labels,
        input_id = session$ns("value")
      )  
    } else {

      # default
      req(n_items() > 0)
      ids <- uuid::UUIDgenerate(n = n_items())
      
      value_ids(ids)
      
      lapply(ids, function(id){
        jsonValueEditUI(session$ns(id))  
      })
      
    }
  })
  
  
  out <- reactiveVal()
  
  observeEvent(value_ids(),{
    
    lis <- list()
    for(i in seq_along(value_ids())){
      print(value()[i])
      lis[[i]] <- callModule(jsonValueEditModule, 
                             value_ids()[i], 
                             type = type(), 
                             value = value()[i])
    }
    
    out(lis)
      
  })
  
  
  order_int <- reactive({
    labels <- labels_to_order_by()
    match(input$value, labels)
  })
  
  
  
  output_vector <- reactive({
    
    if(type() == "order"){
      jsonlite::toJSON(order_int())  
    } else {
      out()
    }
    
  })
  
return(output_vector)
}






library(colourpicker)
library(sortable)
library(shiny)
library(dplyr)

ui <- fluidPage(
  
  tags$div(style = "margin: auto; width: 400px;",
           selectInput("sel_type", NULL, 
                       choices = c("Text" = "text",
                                   "Integer" = "integer",
                                   "Color" = "color",
                                   "Order" = "order",
                                   "JSON" = "json")),
           jsonEditVectorUI("test")
  ),
  tags$hr(),
  verbatimTextOutput("txt")
)

tab <- tibble(
  name = c("Aaap","Banaan","Tak","Kop","Hok"),
  order = 1:5
)

server <- function(input, output, session) {
  output$txt <- renderPrint(out())
  
  out <- callModule(jsonEditVectorModule, "test", 
                    type = reactive(input$sel_type),
                    data = reactive(tab),
                    column = reactive("order"))
}

shinyApp(ui, server)

