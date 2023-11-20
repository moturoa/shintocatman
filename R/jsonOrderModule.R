
#' Shiny JSON order edit module
#' @description Editor for ordering a JSON vector e.g. '[1,2,3,4]'
#' @rdname jsonOrder
#' @export
jsonOrderModuleUI <- function(id){
  
  ns <- NS(id)
  
  uiOutput(ns("ui_rank_list"))
  
}



#' @rdname jsonOrder
#' @export
#' @importFrom sortable rank_list
jsonOrderModule <- function(input, output, session,
                            data = reactive(NULL),
                            order_column = reactive(NULL),
                            label_column = reactive(NULL)
                            ){
  
  
  output$ui_rank_list <- renderUI({
    sortable::rank_list(
      text = "",
      labels = labels(),
      input_id = session$ns("value")
    )
  })
  
  order_data <- reactive({
    from_json(data()[[order_column()]])  
  })
  
  labels <- reactive({
    
    req(order_data())
    req(length(order_data()) > 0)
    if(is.null(label_column())){
      as.character(seq(1, length(order_data())))
    } else {
      from_json(data()[[label_column()]])[order_data()]
    }
  })

  order_int <- reactive({
    order_data()[match(input$value, names(labels()))]
  })
  
  out <- reactive({
    to_json(as.list(order_int()))  
  })
  

return(out)
}




test_jsonOrderModule <- function(){
  devtools::load_all()
  
  
  ui <- softui::simple_page(style = "margin: auto; width: 600px;",
    jsonOrderModuleUI("test"),
    tags$hr(),
    verbatimTextOutput("out"),
  )
  
  server <- function(input, output, session) {
    
    out <- callModule(jsonOrderModule, "test",
               
               data = reactive({  
                 tibble(
                  labels = to_json(setNames(list("een","twee","drie","vier","vijf","zes","zeven"),as.character(1:7))),
                  order = to_json(c(7:1))
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


