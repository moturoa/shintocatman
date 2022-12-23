

library(shiny)

mcekey <- "v57hwcvdkp34j4gn9qh58e55p8y7c5bx1lgzkskcc0ug414z"
options(mce_api_key = mcekey)


uimodule <- function(id){
  
  ns <- NS(id)
  
  uiOutput(ns("ui_main"))
  
}


servermodule <- function(input, output, session){
  
  ns <- session$ns
  
  output$ui_main <- renderUI({
    softui::action_button(ns("openmodal"), "Go modal!", class = "btn-success")
  })
  
  observeEvent(input$openmodal, {
    showModal(
      softui::modal(
        shintocatman::htmlInput(ns("mytext"), "Dit is een test",
                                value = shinipsum::random_text(nwords = 1),
                                height = 600,
                                plugins = c("lists","insertdatetime","autoresize"))
      )
    )
  })
  
}


ui <- softui::simple_page(
  
  
  shintocatman::useHtmlInput(),
  
  
  uimodule("test")
  
)

server <- function(input, output, session) {
  

  callModule(servermodule, "test")
}

shinyApp(ui, server)
