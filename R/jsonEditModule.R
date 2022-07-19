#' Shiny JSON edit module
#' @description Editor for key-value type JSON fields. Edit either or both of the keys and the labels.
#' @param options Reactive vector, with none, one or both of 'delete', 'add'. Controls whether
#' categories can be added and (the last one) removed in the editor.
#' @param options_labels Labels for the buttons in a named vector
#' @param edit Reactive vector with 'key', 'value' (one or both). Controls whether
#' both the key and the value can be edited.
#' @param widths Vector of bootstrap column widths of key and value (must add up to 12)
#' @param value Reactive vector (or reactive(NULL)) with values to place in the editor
#' @param
#' @param input Shiny input, leave alone
#' @param output Shiny output, leave alone
#' @param session Shiny session, leave alone
#' @rdname jsonEdit
#' @export
jsonEditModuleUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    shiny::uiOutput(ns("ui_edit")),    # categories / labels
    shiny::uiOutput(ns("ui_options"))  # add/delete buttons
  )
  
}

#' @rdname jsonEdit
#' @export
#' @importFrom stringr str_remove_all
jsonEditModule <- function(input, output, session, 
                           options = reactive(c("delete","add")),
                           options_labels = c(delete = "Laatste verwijderen", 
                                              add = "Toevoegen", 
                                              save = "Opslaan"),
                           edit = reactive(c("key","value")),
                           widths = c(6,6),
                           value = reactive(NULL)){
  
  
  output$ui_options <- renderUI({
    
    tagList(
      if("add" %in% options()){
        softui::action_button(session$ns("btn_add_cat"), options_labels[["add"]], 
                              status = "secondary",
                              icon = softui:::bsicon("plus-lg"))
      },
      if("delete" %in% options()){
        softui::action_button(session$ns("btn_del_cat"), options_labels[["delete"]], 
                              icon = softui::bsicon("dash-lg"), status = "warning")  
      }
    )
    
  })
  
  
  value_txt <- reactive({
    req(value())
    
    out <- jsonlite::fromJSON(value())
    
    if(!is.list(value()) && stringr::str_remove_all(value(), "\"") == ""){
      out <- jsonlite::fromJSON("[]")
    }
    out
    
  })
  
  n_cat <- reactiveVal()
  observeEvent(value_txt(),{
    n_cat(length(value_txt()))
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
  
  observeEvent(n_cat(), {
    
    n <- n_cat()
    
    for(i in seq_len(n)){
      id <- paste0("val_",i)
      updateTextInput(session, id, value = input[[id]])
    }
    
  })
  
  
  txt_out <- reactive({
    
    val <- value_txt()
    n <- n_cat()
    req(n>0)
    
    for(i in 1:n){
      newval <- input[[paste0("val_",i)]]
      if(!is.null(newval)){
        val[[i]] <- newval  
      }
    }  
    
    for(i in 1:n){
      newkey <- input[[paste0("key_",i)]]
      if(!is.null(newkey)){
        names(val)[i] <- newkey  
      }
      
    }  
    
    val <- val[1:n_cat()]  # is dit nodig?
    
    jsonlite::toJSON(as.list(val), auto_unbox = TRUE)
    
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
                              verbatimTextOutput("txt_out"),
                              tags$hr(),
                              
                              softui::action_button("btn_go_modal", "in modal", status = "success"),
                              verbatimTextOutput("txt_out2")
                            )
                            
                            
  )
  
  server <- function(input, output, session) {
    
    
    edited_txt <- callModule(jsonEditModule, "test", 
                             options = reactive(c("add","delete")),
                             edit = reactive("value"),
                             widths = c(2,10),
                             value = reactive(input$sel_val))
    
    edited_txt2 <- softui::modalize(trigger_open = reactive(input$btn_go_modal),
                                    ui_module = jsonEditModuleUI,
                                    server_module = jsonEditModule,
                                    server_pars = list(
                                      options = reactive(c("add","delete")),
                                      edit = reactive("value"),
                                      widths = c(2,10),
                                      value = reactive(input$sel_val)
                                    )
    )                      
    
    
    
    output$txt_out <- renderPrint({
      edited_txt()
    }) 
    
    output$txt_out2 <- renderPrint({
      edited_txt2()
    }) 
  }
  
  shinyApp(ui, server)
  
}




