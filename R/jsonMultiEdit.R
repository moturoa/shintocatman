

#' JSON Multi-edit for a nested JSON list
#' @description Use case: choice of a selectinput might depend on what is chosen in another
#' selectinput. This nested dependency can be configured with jsonMultiEdit
#' `r lifecycle::badge('experimental')`
#' @rdname jsonMultiEdit
#' @export
jsonMultiEditUI <- function(id, 
                            ui_header = NULL,
                            choices_label = "Keuzelijst",
                            select_label = "Categorie"){
  
  ns <- NS(id)
  
  tags$div(
    ui_header,
    #selectInput(ns("sel_key"), select_label, choices = NULL),
    
    uiOutput(ns("ui_key_select")),
    
    tags$label(choices_label),
     
    uiOutput(ns("ui_listeditor"))
    
  )
  
}

#' @rdname jsonMultiEdit
#' @export
jsonMultiEdit <- function(input, output, session, 
                          edit_names = FALSE,
                          key = reactive("[]"),
                          value = reactive("[]"),
                          select_label = "Categorie",
                          json = TRUE){
  

  # TODO generic convert from JSON?
  value_txt <- reactive({
    
    req(value())

    out <- value()
    if(json){
      out <- jsonlite::fromJSON(out)
    }

    if(!is.list(out) && stringr::str_remove_all(out, "\"") == ""){
      out <- list()
    }
    out

  })

  keys_data <- reactive({
    req(key())
    out <- key()
    if(json){
      out <- jsonlite::fromJSON(out)
    }
    out
  })

  
  #freezeReactiveValue(input, "sel_key")
  
  output$ui_key_select <- renderUI({

  #observe({
    val <- keys_data()
    
    # switch names and values
    nms <- names(val)
    val <- unname(unlist(val))
    val <- setNames(nms,val)
    
    #updateSelectInput(session, "sel_key", choices = val)
    selectInput(session$ns("sel_key"), select_label, choices = val)

  })


  output$ui_listeditor <- renderUI({

    
    keys <- keys_data()

    lapply(names(keys), function(key){

      print(paste("KEY:",key))
      shinyjs::hidden(
        tags$div(id = session$ns(paste0("box_",key)),
          listEditModuleUI(session$ns(paste0("module_",key)))
        )
      )

    })

  })

  observeEvent(input$sel_key, {

    key <- input$sel_key
    all_ids <- paste0("box_",names(keys_data()))
    lapply(all_ids, shinyjs::hide)
    this_id <- paste0("box_",key)
    
    print(paste("Showing:",this_id))
    shinyjs::show(this_id)

  })


  edit_arrays <- reactive({

    print("edit_arrays")
    
    lapply(names(keys_data()), function(key){
      vals <- value_txt()[[key]]
      print(paste("edit_arrays:",key))
      names(vals) <- as.character(seq_along(vals))

      callModule(listEditModule, paste0("module_",key),
                 data = reactive(as.list(vals)),
                 edit_name = edit_names, show_name = edit_names,
                 widths = c(6,6),
                 options = reactive(c("add","delete")),
                 options_labels = c(delete = "Laatste verwijderen",
                                    add = "Toevoegen"))

    })

  })


  edit_output <- reactive({
    val <- lapply(edit_arrays(), function(x)x())
    names(val) <- names(keys_data())
    jsonlite::toJSON(val)
  })


  return(edit_output)
}




test_jsonMultiEdit <- function(){
  
  devtools::load_all()
  
  ui <- softui::simple_page(
    softui::box(width=4,
                
          jsonMultiEditUI("test"),
          
          verbatimTextOutput("txt_out1")
          
          
    ),
    softui::box(width=4,
                
                softui::action_button("btn_modal", "Open in modal", status = "success"),
          verbatimTextOutput("txt_out")
    )
  )
  
  keys <- jsonlite::toJSON(list("1" = "Aap", "2" = "Boom", "3" = "Hond"))
  vals <- jsonlite::toJSON(list("1" = c("Chimpansee", "Gorilla"),
                                "2" = c("Eik", "Beuk"),
                                "3" = "Stabij"))
  
  # vals <- jsonlite::toJSON(list("1" = list(), 
  #                               "2" = list(), 
  #                               "3" = list()))
  
  
  server <- function(input, output, session) {
    
      
    observeEvent(input$btn_modal, {
      
      showModal(
        softui::modal(id_confirm = "btn_confirm",
          title = "Test",
          jsonMultiEditUI(session$ns("nestedselect")),
        )
      )
      
    })

    outmod_edit <- callModule(jsonMultiEdit, "nestedselect",
                         key = reactive(keys),
                         value = reactive(vals),
                         json = TRUE)
    
    out <- callModule(jsonMultiEdit, "test",
                      key = reactive(keys),
                      value = reactive(vals),
                      json = TRUE)
    #
    # out <- callModule(jsonMultiEdit, "test",
    #                   key = reactive(jsonlite::fromJSON(keys)),
    #                   value = reactive(jsonlite::fromJSON(vals)),
    #                   json = FALSE)
   
      
    outmod <- reactiveVal()
    observeEvent(input$btn_confirm, outmod(outmod_edit()))
    
    output$txt_out <- renderPrint({
      outmod_edit()
    })
    
    output$txt_out1 <- renderPrint({
      out()
    })
  }
  
  shinyApp(ui, server)
  
}




