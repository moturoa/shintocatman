
library(softui)

selUI <- function(id){
  
  ns <- NS(id)
  
  selectInput(ns("sel"), "Select", choices = LETTERS)  
  
}



selServer <- function(input, output, session){
  
  
  reactive(input$sel)
  
}




# modalize_button <- function(id, ...){
#   
#   ns <- NS(id)
#   softui::action_button(ns("btn_modal_open"), ...)
#   
# 



modalize <- function(input,output,session,
                     trigger_open = reactive(NULL), 
                     ui_module,
                     ui_pars = list(),
                     
                     server_module,
                     server_pars = list(),  
                     ...){
  
  id <- uuid::UUIDgenerate()
  
  observeEvent(trigger_open(), {
    
    showModal(
      softui::modal(
        id_confirm = "btn_modal_ok", ...,
        do.call(ui_module, c(list(id = session$ns(id)), ui_pars))
      )
    )
    
  })
  
  module_out <- do.call(callModule, c(list(module = server_module, id = id), 
                                      server_pars))
  
  out <- eventReactive(input$btn_modal_ok, {
    module_out()
  })
  
return(out)

  
}






ui <- softui::simple_page(
  
  softui::box(title = "Modalize", width = 6,
              softui::action_button("btn", "Go!", status = "success"),
              tags$hr(),
              verbatimTextOutput("txt_out")
  )
)



server <- function(input, output, session){
  
  out <- callModule(modalize,"noid",
                    trigger_open = reactive(input$btn),
                    ui_module = selUI,
                    server_module = selServer,
                    title = "Dit is modalize!")

  
  output$txt_out <- renderPrint({
    out()
  })
}

shinyApp(ui, server)



