

jsonEditModuleUI <- function(id, class = "", 
                             opties = c("verwijderen","toevoegen")){
  
  ns <- NS(id)
  
  tags$div(class = class,
    
      uiOutput(ns("ui_edit")),
    
      actionButton(ns("btn_add_cat"), "Toevoegen", 
                             icon = softui:::bsicon("plus")),
      if("verwijderen" %in% opties){
        actionButton(ns("btn_del_cat"), "Laatste verwijderen", 
                     icon = softui::bsicon("minus"), class = "btn-warning")  
      },
      if("toevoegen" %in% opties){
        actionButton(ns("btn_save"), "Opslaan", 
                     class = "btn-success",
                     icon = softui::bsicon("save-fill"))  
      }
  )
  
}

jsonEditModule <- function(input, output, session, value = reactive(NULL)){
  
  
  value_txt <- reactive({
    jsonlite::fromJSON(value())
  })
  
  n_cat <- reactiveVal()
  observeEvent(value_txt(),{
    n_cat(length(value_txt()))
  })
  
  output$ui_edit <- renderUI({
    
    val <- value_txt()
    n <- n_cat()
    req(n>0)
    
    lapply(1:n, function(i){
     
      val <- ifelse(i > length(val), "", val[[i]])
      
      fluidRow(
        column(3, i),
        column(9, textInput(session$ns(paste0("txt_",i)), NULL, value = val))
      )
      
    })
    
  })
  
  
  observeEvent(input$btn_del_cat, {
    
    n_cat(n_cat() - 1)  
    
  })
  
  observeEvent(input$btn_add_cat, {
    
    n_cat(n_cat() + 1)  
    
  })
  
  txt_out <- reactiveVal()
  
  observeEvent(input$btn_save, {
    
    val <- value_txt()
    n <- n_cat()
    req(n>0)
    for(i in 1:n){
      val[[i]] <- input[[paste0("txt_",i)]]   
    }
    val <- val[1:n_cat()]
    
    txt_out(jsonlite::toJSON(val, auto_unbox = TRUE))
    
  })
  
return(txt_out)
}




test_jsonedit <- function(){
  
  library(shiny)
  
  ui <- fluidPage(style = "margin: auto; width: 600px;",
                  
                  inlineCSS("
                            .mylist{border: 1px solid hotpink;}
                            "),
      actionButton("btn_edit", "Edit value"),
      verbatimTextOutput("txt_out")
      
  )
  
  server <- function(input, output, session) {
    
    
    observeEvent(input$btn_edit, {
      showModal(
        modalDialog(
          title = "Edit",
          jsonEditModuleUI("test", class = "mylist", opties = "toevoegen"),
          footer = tagAppendAttributes(modalButton("Cancel"), class = "btn-danger")
        )
      )
    })
    
    edited_txt <- callModule(jsonEditModule, "test", 
                             value = reactive("{\"1\":\"\",\"2\":\"Gereed\",\"3\":\"In aanbouw genomen\",\"4\":\"Intake\",\"5\":\"Onherroepelijk plan/besluit\",\"6\":\"Ontwerpplan/besluit\",\"7\":\"Planologische voorbereiding\",\"8\":\"Potentieel\",\"9\":\"Start Project\",\"10\":\"Vastgesteld plan/besluit\",\"11\":\"Vergunning verleend\"}"))
   
    observeEvent(edited_txt(), {
      removeModal()
    })
    
    output$txt_out <- renderPrint({
      edited_txt()
    }) 
  }
  
  shinyApp(ui, server)
  
}




