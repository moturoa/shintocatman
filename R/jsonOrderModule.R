
#' Shiny JSON order edit module
#' @description Editor for ordering a JSON vector e.g. [1,2,3,4]
#' @param class CSS class of the editor
#' @param opties "verwijderen", "toevoegen", or both
#' @param input
#' @rdname jsonOrder
#' @export
jsonOrderModuleUI <- function(id, label = "Change order", icon = NULL, status = "secondary"){
  
  ns <- NS(id)
  
  softui::action_button(ns("btn"),label, icon = icon, status = status)
  
}



#' @rdname jsonOrder
#' @export
#' @importFrom sortable rank_list
jsonOrderModule <- function(input, output, session,
                            data = reactive(NULL),
                            order_column = reactive(NULL),
                            label_column = reactive(NULL),
                            title = "Drag to change the order",
                            header_ui = NULL,
                            callback = function(data){}
                            ){
  
  order_data <- reactive({
    from_json(data()[[order_column()]])  
  })
  
  labels <- reactive({
    req(order_data())
    if(is.null(label_column())){
      as.character(seq(1, length(order_data())))
    } else {
      from_json(data()[[label_column()]])[order_data()]
    }
  })
    
  observeEvent(input$btn, {
    
    showModal(
      softui::modal(
        title = title,
        id_confirm = "btn_confirm", close_txt = "Annuleren",
        
        header_ui,
        
        sortable::rank_list(
          text = "",
          labels = labels(),
          input_id = session$ns("value")
        )
        )
      )
    
  })
  
  order_int <- reactive({
    match(input$value, names(labels()))
  })
  
  output_vector <- reactiveVal()
  
  observeEvent(input$btn_confirm, {
    
    out <- to_json(as.list(order_int()))  
    callback(out)
    
    output_vector(out)
    
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
               
               data = reactive({
                 tibble(
                  labels = to_json(setNames(list("aap","banaan","tak","boom"),as.character(1:4))),
                  order = to_json(c(4,1,2,3))
                )
               }),
               order_column = reactive("order"),
               label_column = reactive("labels")
               )
    
    output$out <- renderPrint({
      out()
    })
    
  }
  
  shinyApp(ui, server)
  

}


