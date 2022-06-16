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
    req(value())
    jsonlite::fromJSON(value())
  })
  
  n_cat <- reactiveVal()
  observeEvent(value_txt(),{
    n_cat(length(value_txt()))
  })

  
  observeEvent(input$btn, {
    
    showModal(
      softui::modal(
        
        id_confirm = "btn_save", confirm_txt = options_labels[["save"]],
        close_txt = "Annuleren",
        
        tags$div(
            uiOutput(session$ns("ui_edit")),
                 
            if("add" %in% options()){
              softui::action_button(session$ns("btn_add_cat"), options_labels[["add"]], 
                                    status = "secondary",
                           icon = softui:::bsicon("plus"))
            },
            if("delete" %in% options()){
              softui::action_button(session$ns("btn_del_cat"), options_labels[["delete"]], 
                          icon = softui::bsicon("minus"), status = "warning")  
            }
        )
      )
    )
    
  })
  
    
  output$ui_edit <- renderUI({
    
    # old values (provided as input)
    val <- value_txt()
    
    # number of categories
    n <- n_cat()
    
    if(n == 0){
      tags$p(glue("Klik {options_labels[['add']]} om een optie toe te voegen"))
    } else {
        
      lapply(1:n, function(i){
       
        key_id <- paste0("key_",i)
        val_id <- paste0("val_",i)
        
        # Read key from input data, or if n_cat > length(provided), set to ""
        key <- ifelse(i > length(val), "", names(val)[i])
      
        #- dit is wel handig omdat we dan edits bewaren zonder eerst naar DB te schrijven
        # maar dit geeft serieuze bugs
        # values already entered, keep it here
        # isolate({
        #   if(!is.null(input[[key_id]])){
        #     key <- input[[key_id]]
        #   }  
        # })
        
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
        # isolate({
        #   if(!is.null(input[[val_id]])){
        #     val <- input[[val_id]]
        #   }  
        # })
        
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
    }
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
  devtools::load_all()
  
  ui <- softui::simple_page(style = "margin: auto; width: 600px;",

          softui::box(
            selectInput("sel_val", "Edit iets", 
                        choices =c("[]",
                                   '{"1":"Banaan","2":"Appel","3":"Aardbei"}',
                                   '{"1":"Hallo","2":"Goedendag","3":"Tot ziens"}')),
                      
                                    
            jsonEditModuleUI("test"),
            verbatimTextOutput("txt_out")
          )                  
                          
                  
      
  )
  
  server <- function(input, output, session) {
    
    
    edited_txt <- callModule(jsonEditModule, "test", 
                             options = reactive(c("add","delete")),
                             edit = reactive("value"),
                             widths = c(2,10),
                             value = reactive(input$sel_val))
                            
   

    output$txt_out <- renderPrint({
      edited_txt()
    }) 
  }
  
  shinyApp(ui, server)
  
}




