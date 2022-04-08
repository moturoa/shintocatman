

library(shiny)

ui <- fluidPage(
  
  actionButton("show", "Show")
  
)

server <- function(input, output, session) {
  
  observeEvent(input$show, {
    
    showModal(
      modalDialog(
        title = "Kies!",
        actionButton("show2", "Show!")
      )
    )
  })
  
  observeEvent(input$show2, {
    
    showModal(
      modalDialog(
        title = "Kies nog een keer!",
        actionButton("show3", "Show!")
      )
    )
  })
  
  observeEvent(input$show3, {
    
    showModal(
      modalDialog(
        title = "tot ziens!",
        tags$p("Doei!")
      )
    )
  })
}

shinyApp(ui, server)