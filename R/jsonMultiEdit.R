

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

  output$ui_key_select <- renderUI({

    val <- keys_data()
    
    # switch names and values
    nms <- names(val)
    val <- unname(unlist(val))
    val <- setNames(nms,val)
    
    selectInput(session$ns("sel_key"), select_label, choices = val)

  })

  
  output$ui_listeditor <- renderUI({

    keys <- keys_data()

    lapply(names(keys), function(key){
      
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
    
    shinyjs::show(this_id)

  })


  edit_arrays <- reactive({

    lapply(names(keys_data()), function(key){
      
      vals <- value_txt()[[key]]
      if(length(vals) > 0){
        names(vals) <- as.character(seq_along(vals))  
      } else {
        vals <- list()
      }
      

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


  outputOptions(output, "ui_key_select", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_listeditor", suspendWhenHidden = FALSE)
  
  return(edit_output)
}




#----- Test module
# (not exported!)
test_jsonMultiEdit <- function(){
  
  devtools::load_all()
  
  ui <- softui::simple_page(
    
    softui::fluid_row(
      softui::box(width=4,
                  
            jsonMultiEditUI("test"),
            
            verbatimTextOutput("txt_out1")
            
            
      ),
      softui::box(width=4,
                  
            softui::modal_action_button("btn_modal", 
                                        "modal_multi",
                                        "Open in modal", status = "success"),
                  
            softui::ui_modal(id = "modal_multi",
                          id_confirm = "btn_confirm",
                          title = "Test",
                          jsonMultiEditUI("nestedselect"),
            ),
                    
            verbatimTextOutput("txt_out")
      ),
      softui::box(width = 4, title = "listEdit",
        
        listEditModuleUI("test_listedit"),
        tags$hr(),
        verbatimTextOutput("txt_listout")
        
      )
  )
  )
  # 
  # keys <- jsonlite::toJSON(list("1" = "Aap", "2" = "Boom", "3" = "Hond"))
  # vals <- jsonlite::toJSON(list("1" = c("Chimpansee", "Gorilla"),
  #                               "2" = c("Eik", "Beuk"),
  #                               "3" = "Stabij"))
  # #vals <- jsonlite::toJSON(list())
  
  
  server <- function(input, output, session) {
    
    
    key_data <- reactiveVal(list("1" = "Aap", "2" = "Boom", "3" = "Hond"))
    val_data <- reactiveVal(list("1" = c("Chimpansee", "Gorilla"),
                                 "2" = c("Eik", "Beuk"),
                                 "3" = "Stabij"))
    
    # just list edit
    list_out <- callModule(listEditModule, "test_listedit",
               data = key_data,
               edit_name = FALSE, 
               show_name = TRUE,
               widths = c(2,8),
               options = reactive(c("add","delete")),
               options_labels = c(delete = "Laatste verwijderen",
                                  add = "Toevoegen"))
    
    output$txt_listout <- renderPrint({
      list_out()
    })
    
    outmod_edit <- callModule(jsonMultiEdit, "nestedselect",
                         key = key_data,
                         value = val_data,
                         json = FALSE)
    
    
    observeEvent(outmod_edit(), {
      invisible()
    })
    
    
    
    out <- callModule(jsonMultiEdit, "test",
                      key = key_data,
                      value = val_data,
                      json = FALSE)
    
    observeEvent(input$btn_confirm, {
      val_data(jsonlite::fromJSON(outmod_edit()))
    })
    
    
    output$txt_out1 <- renderPrint({
      out()
    })
  }
  
  shinyApp(ui, server)
  
}




