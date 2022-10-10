

valueEditModuleUI <- function(id, name = "1", 
                              edit_name = TRUE, 
                              show_name = TRUE,
                              value = 1, widths = c(6,6)){
  
  ns <- NS(id)
  
  name_edit <- textInput(ns("name"), NULL, value = name, width = "100%")
  
  if(show_name & !edit_name){
    name_edit <- shinyjs::disabled(name_edit)
  }
  
  val_edit <- textInput(ns("value"), NULL, value = value, width = "100%")
  
  if(!show_name)widths <- c(0,12)
  
  softui::fluid_row(id = id,
    if(widths[1] > 0)column(widths[1], name_edit),
    column(widths[2], val_edit)
  )  
  
}



valueEditModule <- function(input, output, session,
                            keep_name = TRUE){
  
  reactive({
    
    if(keep_name){
       setNames(input$value,input$name)
    } else {
       input$value
    }
  })
  
  
}


test_valueEditModule <- function(){
  
  library(shiny)
  library(softui)
  
  ui <- softui::simple_page(
    softui::box(width=4,
                
      valueEditModuleUI("one", name = "Aap", value = "Gorilla", edit_name = TRUE, show_name = TRUE),
      verbatimTextOutput("one_out"),
      valueEditModuleUI("two", name = "Aap", value = "Gorilla", edit_name = FALSE, show_name = TRUE),
      verbatimTextOutput("two_out"),
      valueEditModuleUI("three", name = NULL, value = "Gorilla", edit_name = FALSE, show_name = FALSE),
      verbatimTextOutput("three_out")
      
    )
  )
  
  server <- function(input, output, session) {
    one <- callModule(valueEditModule,"one", keep_name = TRUE)
    two <- callModule(valueEditModule,"one", keep_name = TRUE)
    three <- callModule(valueEditModule,"one", keep_name = FALSE)
    
    
    output$one_out <- renderPrint({one()})
    output$two_out <- renderPrint({two()})
    output$three_out <- renderPrint({three()})
  }
  
  shinyApp(ui, server)
  
}

#test_valueEditModule()







#' Module to edit a vector
#' @description Like [jsonEditModule()] but without bugs. Also does not use JSON,
#' but vector in / vector out.
#' `r lifecycle::badge('experimental')`
#' @export
#' @rdname listEditModule
listEditModuleUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(
    class = "shintocatman_list_edit_module",
    
    uiOutput(ns("ui_label")),
    tags$div(id = ns("list_edit_placeholder")),
    uiOutput(ns("ui_options"))
  )
   
}


#' @export
#' @rdname listEditModule
listEditModule <- function(input, output, session, data = reactive(list()),
                           edit_name = TRUE, show_name = TRUE,
                           widths = c(6,6),
                           options = reactive(c("add","delete")),
                           options_labels = c(delete = "Laatste verwijderen", 
                                              add = "Toevoegen", 
                                              save = "Opslaan")
                           ){
  
  output$ui_options <- renderUI({

    
    print("ui_options")
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
  
  n_values <- reactive({
    length(data())
  })
  
  out <- reactiveValues()
  
  input_ids <- reactiveVal()
  
  
  # Initial data to edit
  observeEvent(data(), {
    
    els <- data()
    
    if(length(input_ids())> 0){
      for(id in input_ids()){
        removeUI(selector = paste0("#",session$ns(id)))
      }
    }
    
    ids <- uuid::UUIDgenerate(n = length(els))
    input_ids(ids)
    
    for(i in seq_along(els)){
      
      id_where <- paste0("#", session$ns("list_edit_placeholder"))
      id_module <- ids[i]
      ui <- valueEditModuleUI(id = session$ns(id_module),
                              name = names(els)[i],
                              value = els[[i]],
                              edit_name = edit_name, 
                              show_name = show_name,
                              widths = widths)
      insertUI(id_where, "beforeEnd", ui = ui)
     
      out[[id_module]] <- callModule(valueEditModule, id_module, 
                                     keep_name = edit_name | show_name)
    }
    
    
  })
  
  # Add a value
  observeEvent(input$btn_add_cat, {
    

    id_where <- paste0("#", session$ns("list_edit_placeholder"))
    id_module <- uuid::UUIDgenerate()
    input_ids(c(input_ids(),id_module))
    
    ui <- valueEditModuleUI(id = session$ns(id_module),
                            name = "",
                            value = "",
                            edit_name = edit_name, 
                            show_name = show_name,
                            widths = widths)
    insertUI(id_where, "beforeEnd", ui = ui)
    
    out[[id_module]] <- callModule(valueEditModule, id_module, 
                                   keep_name = edit_name | show_name)
  })
  
  
  observeEvent(input$btn_del_cat, {
    
    # hier kunnen we ook andere verwijderen uiteindelijk...
    ids <- input_ids()
    lastid <- ids[length(ids)]
    
    removeUI(selector = paste0("#",session$ns(lastid)))
    input_ids(ids[-length(ids)])
    
  })
  
  
  output$ui_label <- renderUI({
    
    if(n_values() == 0){
      "Nog geen waardes! Klik op 'Toevoegen'"
    }
    
  })
  
  
  val_out <- reactive({
    val_list <-lapply(reactiveValuesToList(out), function(x)x())

    # Sort using the IDs that we tracked; this is some sort of weird
    # sideeffect of reactiveValues or reactiveValuesToList (order is shuffled)
    val_list <- val_list[input_ids()]
    
    do.call(c,unname(val_list))
  })
  
return(val_out)
}



test_listEditModule <- function(){
  
  devtools::load_all()
  library(shiny)
  library(softui)
  
  ui <- softui::simple_page(
    softui::box(width = 4,
      listEditModuleUI("test")
    ),
    softui::box(width = 4, 
      verbatimTextOutput("txt_out")            
    )
  )
  
  server <- function(input, output, session) {
    testit <- callModule(listEditModule, "test", 
               data = reactive(list(a = 1, b = 2)),
               edit_name = TRUE, show_name = TRUE
    )
    
    output$txt_out <- renderPrint({
      testit()
    })
  }
  
  shinyApp(ui, server)
  
}

#test_listEditModule()










