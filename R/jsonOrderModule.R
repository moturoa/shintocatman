
#' Shiny JSON order edit module
#' @description Editor for ordering a JSON vector e.g. [1,2,3,4]
#' @param class CSS class of the editor
#' @param opties "verwijderen", "toevoegen", or both
#' @param input
#' @rdname jsonOrder
#' @export
jsonOrderModuleUI <- function(id){
  
  ns <- NS(id)
  
  uiOutput(ns("input_field"))
  
}



#' @rdname jsonOrder
jsonOrderModule <- function(input, output, session,
                            data = NULL,
                            order_column = NULL,
                            label_column = NULL
                            ){
  
  order_data <- from_json(data[[order_column]])
  labels <- from_json(data[[label_column]])
  
  output$input_field <- renderUI({
    rank_list(
      text = "Change the order",
      labels = labels,
      input_id = session$ns("value")
    )  
  })
  
  order_int <- reactive({
    match(input$value, names(labels))
  })
  
  
  output_vector <- reactive({

    req(order_int())
    to_json(as.list(order_int()))
   
  })
  
return(output_vector)
}




test_jsonOrderModule <- function(){
  library(shiny)
  
  ui <- fluidPage(style = "margin: auto; width: 600px;",
    jsonOrderModuleUI("test"),
    tags$hr(),
    verbatimTextOutput("out")
  )
  
  server <- function(input, output, session) {
    
    out <- callModule(jsonOrderModule, "test",
               
               data = tibble(
                 labels = to_json(setNames(list("aap","banaan","tak","boom"),as.character(1:4))),
                 order = to_json(setNames(as.list(1:4),1:4))
               ),
               order_column = "order",
               label_column = "labels"
               )
    
    output$out <- renderPrint({
      out()
    })
    
  }
  
  shinyApp(ui, server)
  

}


