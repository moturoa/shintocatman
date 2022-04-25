#' Shiny JSON edit module
#' @description Editor for key-value type JSON fields. Edit either or both of the keys and the labels.
#' @param class CSS class of the editor
#' @param opties "verwijderen", "toevoegen", or both
#' @param input
#' @rdname jsonEdit
#' @export
jsonEditModuleUI <- function(id, class = "", icon = NULL, label = "Edit values"){
  
  ns <- NS(id)
  actionButton(ns("btn"),label, icon = icon, class = class)
  
}

#' @rdname jsonEdit
#' @export
jsonEditModule <- function(input, output, session, 
                           options = reactive(c("delete","add")),
                           options_labels = c(delete = "Laatste verwijderen", 
                                              add = "Toevoegen", 
                                              save = "Opslaan"),
                           edit = reactive(c("key","value")),
                           widths = c(6,6),
                           value = reactive(NULL),
                           callback = function(data){}){
  
  
  value_txt <- reactive({
    jsonlite::fromJSON(value())
  })
  
  n_cat <- reactiveVal()
  observeEvent(value_txt(),{
    n_cat(length(value_txt()))
  })

  
  observeEvent(input$btn, {
    
    
    showModal(
      modalDialog(
        
        tags$div(
          uiOutput(session$ns("ui_edit")),
                 
                 if("add" %in% options()){
                   actionButton(session$ns("btn_add_cat"), options_labels[["add"]], 
                                icon = softui:::bsicon("plus"))
                 },
                 if("delete" %in% options()){
                   actionButton(session$ns("btn_del_cat"), options_labels[["delete"]], 
                                icon = softui::bsicon("minus"), class = "btn-warning")  
                 },
                 
                 actionButton(session$ns("btn_save"), options_labels[["save"]], 
                              class = "btn-success",
                              icon = softui::bsicon("save-fill"))
        )
      )
    )
    
  })
  
    
  output$ui_edit <- renderUI({
    
    # old values (provided as input)
    val <- value_txt()
    
    # number of categories
    n <- n_cat()
    req(n>0)
    
    lapply(1:n, function(i){
     
      key_id <- paste0("key_",i)
      val_id <- paste0("val_",i)
      
      # Read key from input data, or if n_cat > length(provided), set to ""
      key <- ifelse(i > length(val), "", names(val)[i])
    
      # values already entered, keep it here
      isolate({
        if(!is.null(input[[key_id]])){
          key <- input[[key_id]]
        }  
      })
      
      # if still no value found, use nothing if editing, or cat nr. when not editing
      if(key == "" & !("key" %in% edit())){
        key <- as.character(i)
      }
      key_edit <- textInput(session$ns(key_id), NULL, value = key, width = "100%")
      if(!("key" %in% edit())){
        key_edit <- shinyjs::disabled(key_edit)
      }
      
      val <- ifelse(i > length(val), "", val[[i]])
      
      # values already entered, keep it here
      isolate({
        if(!is.null(input[[val_id]])){
          val <- input[[val_id]]
        }  
      })
      
      if(val == "" & !("value" %in% edit())){
        val <- as.character(i)
      }
      val_edit <- textInput(session$ns(val_id), NULL, value = val, width = "100%")
      if(!("value" %in% edit())){
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
    
    for(i in 1:n){
      val[[i]] <- input[[paste0("val_",i)]]   
    }  
  
    for(i in 1:n){
      names(val)[i] <- input[[paste0("key_",i)]]   
    }  

    val <- val[1:n_cat()]  # is dit nodig?
    
    valjson <- jsonlite::toJSON(as.list(val), auto_unbox = TRUE)
    
    txt_out(valjson)
    
    removeModal()
    callback()
  })
  
return(txt_out)
}




test_jsonedit <- function(){
  
  library(shiny)
  library(softui)
  library(shinyjs)
  
  ui <- softui::simple_page(style = "margin: auto; width: 600px;",

                  jsonEditModuleUI("test"),
      verbatimTextOutput("txt_out")
      
  )
  
  server <- function(input, output, session) {
    
    
    edited_txt <- callModule(jsonEditModule, "test", 
                             options = reactive(c("add","delete")),
                             edit = reactive("value"),
                             widths = c(2,10),
                             value = reactive("[]"))
                               #reactive("{\"1\":\"\",\"2\":\"Gereed\",\"3\":\"In aanbouw genomen\",\"4\":\"Intake\",\"5\":\"Onherroepelijk plan/besluit\",\"6\":\"Ontwerpplan/besluit\",\"7\":\"Planologische voorbereiding\",\"8\":\"Potentieel\",\"9\":\"Start Project\",\"10\":\"Vastgesteld plan/besluit\",\"11\":\"Vergunning verleend\"}"))
   

    output$txt_out <- renderPrint({
      edited_txt()
    }) 
  }
  
  shinyApp(ui, server)
  
}




