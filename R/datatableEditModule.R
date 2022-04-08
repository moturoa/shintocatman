



datatableEditModuleUI <- function(id){

  ns <- NS(id)
  
  tagList(
    # tags$script(
    #   HTML("$('body').on('shown.bs.modal', (x) => 
    #                $(x.target).find('input[type=\"text\"]:first').focus())")
    # ),

    actionButton(ns("btn_add_row"),
                   "Rij toevoegen",
                   icon = bsicon("plus"),
                   class = "btn-light"),
    
    shinyjs::disabled(
      actionButton(ns("btn_edit_cell"), "Bewerken", icon = bsicon("pencil-square"), class = "btn-light")
    ),
    
    shinyjs::hidden(
      actionButton(ns("btn_remove_row"), "Verwijder", icon = bsicon("minus"), class = "btn-warning")
    ),
    
    DT::dataTableOutput(ns("dt_main"))
    # tags$hr(),
    # verbatimTextOutput(ns("txt_out"))
  )
  
  
    
  
}


datatableEditModule <- function(input, output, session, 
                                table, 
                                table_callback = function(data)data,
                                id_column = "id",
                                no_edit = c("id")){

  # output$txt_out <- renderPrint({
  #   reactiveValuesToList(input)
  # })
  
  db_ping <- reactiveVal()
  
  
  data_formatted <- reactive({
    db_ping()
    .db$read_table(table) %>%
      table_callback() %>%
      arrange(!!sym(id_column))
  })
  
  
  selected_cell <- reactive({
    cl <- input$dt_main_cells_selected
    if(nrow(cl) == 0)return(NULL)
    
    list(
      row = cl[1,1],
      col = cl[1,2] + 1   # indexes on zero!
    )
  })
  
  # index value of the row of the selected cell
  idx_value <- reactive({
    cl <- selected_cell()
    
    if(is.null(cl)){
      return(NULL)
    } else {
      data_formatted() %>% 
        dplyr::slice(cl$row) %>% 
        dplyr::pull(!!sym(id_column))  
    }
    
    
  })
  
  # name of the column of the selected cell
  edit_column <- reactive({
    cl <- selected_cell()
    req(cl)
    names(data_formatted())[cl$col]  
  })
  
  old_value <- reactive({
    cl <- selected_cell()
    req(cl)
    data <- as.data.frame(data_formatted())
    data[cl$row, cl$col]
  })
  
  
  observe({
    cl <- input$dt_main_cells_selected
    
    shinyjs::toggleState("btn_edit_cell", condition = nrow(cl)>0)
    shinyjs::toggle("btn_remove_row", condition = nrow(cl)>0)
  })
  

  
  
  
  output$dt_main <- DT::renderDT({
    
    isolate(data_formatted()) %>%
      softui::datatafel(dom = "tp", 
                        selection = list(mode="single", target="cell"), 
                        pageLength = 25, 
                        scrollX = TRUE, 
                        extensions = list(), 
                        ordering = FALSE)
    
  })
  
  observe({
      prox <- DT::dataTableProxy("dt_main")
      DT::replaceData(prox, data_formatted(), 
                      resetPaging = FALSE,
                      rownames = FALSE)
  })
  
  observeEvent(input$btn_add_row, {
    
    
    .db$insert_empty_row(table)
    db_ping(runif(1))
    
    # showModal(
    #   modalDialog(
    #     fade = FALSE,
    #     easyClose = FALSE,
    #     title = tagList(bsicon("plus"), "Rij toevoegen"),
    #     
    # 
    #     textInput(session$ns("txt_dataset"), "Dataset"),
    #     textInput(session$ns("txt_filename"), "Filename"),
    #     textInput(session$ns("txt_path"), "Path"),
    #     
    #     
    #     footer = tagList(
    #       actionButton("xyz", "Annuleren", 
    #                    icon = icon("minus"), 
    #                    class= "btn-danger ontop",
    #                    `data-dismiss` = "modal"),
    #       actionButton(session$ns("btn_confirm_add_row"),
    #                    "OK", icon = bsicon("check"),
    #                    class = "btn-success")
    #     )
    #   )
    # )
    # 
    
  })
  
  # observeEvent(input$btn_confirm_add_row, {
  #   
  #   .db$add_sourceconfig(
  #     input$txt_dataset,
  #     input$txt_filename,
  #     input$txt_path
  #   )
  #   
  #   db_ping(runif(1))
  #   removeModal()
  # })
  
  
  
  observeEvent(input$btn_remove_row, {
    
    showModal(
      modalDialog(
        size = "m",
        title = "Rij verwijderen?",
        
        
        tags$p("Je gaat deze hele rij verwijderen, weet je het zeker?"),
        
        footer = tagList(
          actionButton("xyz", "Annuleren", 
                       icon = icon("minus"), 
                       class= "btn-danger ontop",
                       `data-bs-dismiss` = "modal"),
          actionButton(session$ns("btn_confirm_delete"),
                       "OK", icon = bsicon("check"),
                       class = "btn-success")
        )
        
      )
    )
    
    
  })
  
  observeEvent(input$btn_confirm_delete, {
    
    id <- idx_value()
    
    .db$delete_rows_where(table, id_column, id)
    toastr_info("Rij verwijderd")
    removeModal()
    
  })
  

  
  
  observeEvent(input$btn_edit_cell, {
    
    cl <- input$dt_main_cell_clicked
    req(cl)
    
    col <- edit_column()
    
    if(col %in% no_edit){
      toastr_info("Deze kolom kan niet bewerkt worden")
      return(NULL) 
    }
    
    oldval <- old_value()
    
    uiInput <- if(is.numeric(oldval))numericInput else textAreaInput
    
    showModal(
      modalDialog(
        title = tagList(bsicon("pencil-edit"), "Edit"),
        uiInput(session$ns("ui_input"), col, value = oldval, width = "100%"),
        
        footer = tagList(
          actionButton("xyz", "Annuleren", 
                       icon = icon("minus"), 
                       class= "btn-danger ontop",
                       `data-bs-dismiss` = "modal"),
          actionButton(session$ns("btn_confirm_edit"),
                       "OK", icon = bsicon("check"),
                       class = "btn-success")
        )
      )
    )

  })
  
  observeEvent(input$btn_confirm_edit, {
    
    if(isTRUE(old_value() == input$ui_input)){
      
      toastr_info("Geen wijziging.")
      
      
    } else {
      .db$replace_value_where(table,
                              col_replace = edit_column(), 
                              val_replace = input$ui_input, 
                              col_compare = id_column,
                              val_compare = idx_value())  
      toastr_success("Wijziging opgeslagen.")
    }
    
    
    removeModal()
    db_ping(runif(1))
  })
  
  
}




