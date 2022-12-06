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
  
  listEditModuleUI(ns("editor"))
  
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
  
  
  list_data <- reactive({
    req(value())

    out <- jsonlite::fromJSON(value())

    if(!is.list(value()) && stringr::str_remove_all(value(), "\"") == ""){
      out <- jsonlite::fromJSON("[]")
    }

    out    
  })
  
  # we are now calling the better version that edits a list
  vec <- callModule(listEditModule, "editor",
             data = list_data,
             widths = widths, 
             edit_name = isolate(ifelse(is.null(edit()),TRUE,"key" %in% edit())),
             show_name = TRUE,
             options = options,
             options_labels = options_labels)
             
  
  vec_json <- reactive({
    req(vec())
    jsonlite::toJSON(as.list(vec()), auto_unbox = TRUE)
  })
  
  
return(vec_json)
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
                              
                              softui::modal_action_button("btn_modal", 
                                                          "modal_multi",
                                                          "Open in modal", status = "success"),
                              softui::ui_modal(id = "modal_multi",
                                               id_confirm = "btn_confirm",
                                               title = "Test",
                                               jsonEditModuleUI("test_inmodal")
                              ),
                              
                              verbatimTextOutput("txt_out2")
                            )
                            
                            
  )
  
  server <- function(input, output, session) {
    
    
    # edited_txt <- callModule(jsonEditModule, "test", 
    #                          options = reactive(c("add","delete")),
    #                          edit = reactive("value"),
    #                          widths = c(2,10),
    #                          value = reactive(input$sel_val))
    
    edited_txt2 <- callModule(jsonEditModule,"test_inmodal",
                              options = reactive(c("add","delete")),
                              edit = reactive("value"),
                              widths = c(2,10),
                              value = reactive(input$sel_val)
    )                      
    
    
    
    # output$txt_out <- renderPrint({
    #   edited_txt()
    # }) 
    # 
    output$txt_out2 <- renderPrint({
      edited_txt2()
    }) 
  }
  
  shinyApp(ui, server)
  
}




