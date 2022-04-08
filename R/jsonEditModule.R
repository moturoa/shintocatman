#' Shiny JSON edit module
#' @param class CSS class of the editor
#' @param opties "verwijderen", "toevoegen", or both
#' @param input
#' @rdname jsonEdit
#' @export
jsonEditModuleUI <- function(id, class = "", 
                             options = c("delete","add"),
                             options_labels = c(delete = "Laatste verwijderen", add = "Toevoegen", save = "Opslaan")
                             ){
  
  ns <- NS(id)
  
  tags$div(class = class,
    
      uiOutput(ns("ui_edit")),
    
      if("add" %in% options){
        actionButton(ns("btn_add_cat"), options_labels[["add"]], 
                             icon = softui:::bsicon("plus"))
      },
      if("delete" %in% options){
        actionButton(ns("btn_del_cat"), options_labels[["delete"]], 
                     icon = softui::bsicon("minus"), class = "btn-warning")  
      },
      
      actionButton(ns("btn_save"), options_labels[["save"]], 
                     class = "btn-success",
                     icon = softui::bsicon("save-fill"))
  )
  
}

#' @rdname jsonEdit
#' @export
jsonEditModule <- function(input, output, session, 
                           edit = c("key","value"),
                           widths = c(6,6),
                           value = reactive(NULL)){
  
  
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
     
      key <- ifelse(i > length(val), "", names(val)[i])
      if(key == "" & !("key" %in% edit)){
        key <- as.character(i)
      }
      key_edit <- textInput(session$ns(paste0("key_",i)), NULL, value = key, width = "100%")
      if(!("key" %in% edit)){
        key_edit <- shinyjs::disabled(key_edit)
      }
      
      val <- ifelse(i > length(val), "", val[[i]])
      if(val == "" & !("value" %in% edit)){
        val <- as.character(i)
      }
      val_edit <- textInput(session$ns(paste0("val_",i)), NULL, value = val, width = "100%")
      if(!("value" %in% edit)){
        val_edit <- shinyjs::disabled(val_edit)
      }
    
      softui::fluid_row(
        column(widths[1], key_edit),
        column(widths[2], val_edit)
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
    if("value" %in% edit){
      for(i in 1:n){
        val[[i]] <- input[[paste0("val_",i)]]   
      }  
    }
    
    if("key" %in% edit){
      for(i in 1:n){
        names(val)[i] <- input[[paste0("key_",i)]]   
      }  
    }
    
    val <- val[1:n_cat()]  # is dit nodig?
    
    txt_out(jsonlite::toJSON(val, auto_unbox = TRUE))
    
  })
  
return(txt_out)
}




test_jsonedit <- function(){
  
  library(shiny)
  
  ui <- fluidPage(style = "margin: auto; width: 600px;",
                  useShinyjs(),
      actionButton("btn_edit", "Edit value"),
      verbatimTextOutput("txt_out")
      
  )
  
  server <- function(input, output, session) {
    
    
    observeEvent(input$btn_edit, {
      showModal(
        modalDialog(
          title = "Edit",
          jsonEditModuleUI("test", options = c("add","delete")),
          footer = tagAppendAttributes(modalButton("Cancel"), class = "btn-danger")
        )
      )
    })
    
    edited_txt <- callModule(jsonEditModule, "test", 
                             edit = "value",
                             widths = c(2,10),
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




