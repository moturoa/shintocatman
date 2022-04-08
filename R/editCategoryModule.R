




editCategoryUI <- function(id){
  
  
  ns <- NS(id)
  
  tags$div(style = "width: 600px;",
    uiOutput(ns("ui_cat_sortable")),
    
    verbatimTextOutput(ns("txt_out"))
  )
  
}

cat_color_picker <- function(...){
  colourpicker::colourInput(..., showColour = "both", allowTransparent = TRUE)
}

editCategoryModule <- function(input, output, session, column_name = reactive(NULL)){
  
  
  db_ping <- reactiveVal()
  
  data_definition <- reactive({
    db_ping()
    .db$read_table("data_definition") %>%
      filter(column_name == !!column_name())
  })
  
  data_cat <- reactive({
    req(data_definition())
    data_definition() %>% 
      pull(categories) %>%
      jsonlite::fromJSON(.)
  })
  
  
  output$ui_cat_sortable <- renderUI({
    
    cat <- data_cat()
    
    rank_list(
      input_id = session$ns("cat_sortable"),
      text = column_name(),
      class = "myranklab",
      
      labels = lapply(1:length(cat), function(i){
        tags$div(style = "height: 100px; border: 1px solid grey; padding: 20px;", 
                 
                 tags$h4(cat[[i]]),
                 cat_color_picker(session$ns(paste0("col_",i)), 
                                  NULL, 
                                  value = "#FFF")
        )
      })
    )
  })
  
  
    
  output$txt_out <- renderPrint({
    input$cat_sortable
  })
  
  
}


test_editcategory <- function(){
  
  source("global.r")
  
  library(colourpicker)
  
  ui <- fluidPage(
    tags$style(
      HTML(
        ".myranklab{
          overflow : visible;
          background-color: white;
        }
        "
      )
    ),
    editCategoryUI("test")
  )
  
  server <- function(input, output, session) {
   out <- callModule(editCategoryModule, "test", column_name = reactive("woningtype")) 
  }
  
  shinyApp(ui, server)

}

