
library(shintocatman)
library(softui)
library(shiny)

options(mce_api_key = 'v57hwcvdkp34j4gn9qh58e55p8y7c5bx1lgzkskcc0ug414z')

ui <- softui::simple_page(
  
  htmlEditModuleUI("test", status = 'success', icon = bsicon("pencil-square")),
  tags$hr(),
  verbatimTextOutput("out")
  
)

server <- function(input, output, session) {
  
  out <- callModule(htmlEditModule, "test", value = reactive("<b>Dit</b> is een <i>test</i>"))
  
  output$out <- renderPrint({
    out()
  })
  
  
}

shinyApp(ui, server)

