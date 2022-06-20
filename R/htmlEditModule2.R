
#' New shiny input for TinyMCE
#' @description Set options(mce_api_key = "MYKEY") before running this. See 1Pass.
#' @export
htmlInput <- function(inputId, value = NULL, ...){
  
  key <- get_mce_apikey()

  value <- restoreInput(id = inputId, default = value)
  
  tagList(
    shiny::singleton(
      tags$script(src = paste0("https://cdn.tiny.cloud/1/",key,"/tinymce/5/tinymce.min.js"))
    ),
    htmltools::htmlDependency(
      name = "tinymcebinding", version = "0.1",
      package = "shintocatman",
      src = c(file = "tinymce"),
      script = "shiny-tinymce-bindings.js",
    ),
    tags$script(glue("tinyMCE.init({selector: '#{{inputId}}',",
                     "inline: false,",
                     "branding: false,",
                     "contextmenu: '',",
                     "menubar: false,",
                     "height: 500",
                     "})", 
                     .open = "{{", .close = "}}")),
    tags$div(style = "width: 100%; height: 500px; padding: 20px; border: 1px solid black;",
             id = inputId, 
             class = "shinytinymce", 
             HTML(value), ...
    )
  )
  
}

get_mce_apikey <- function(){
  opt <- getOption("mce_api_key", default = NULL)
  if(is.null(opt)){
    stop("Set options(mce_api_key = '<<TINYMCE API KEY>>')")
  }
  opt
}


# werkt niet meer ...
# updateTinyMCE <- function(session, inputId, content){
#   data_list <- list(id = inputId, content = content)
#   session$sendCustomMessage(type = "shinyMCE.update", data_list)
# }


test_htmlEditModule2 <- function(){
  
  library(softui)
  
  options(mce_api_key = "MYKEY")
  
  ui <- softui::simple_page(
    
    softui::box(width = 6,
                
                htmlInput("test", value = "<b>Hallo!</b>, dit is een <h4>TEST</h4>")
                
                ),
    actionButton("btn1", "Update"),
    verbatimTextOutput("txt_out")
    
  )
  
  server <- function(input, output, session) {
    
    observeEvent(input$btn1, {
      updateTinyMCE(session, "test", "Dit is de nieuwe tekst")
    })
    
    output$txt_out <- renderPrint({
      input$test
    })
    
  }
  
  shinyApp(ui, server)
  
  
}


