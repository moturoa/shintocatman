
#' Shiny HTML input field 
#' @description A rich text editor for Shiny applications with the TinyMCE editor.
#' @details Set options(mce_api_key = "MYKEY"). Get your free API key at [www.tiny.cloud]
#' @param inputId The Shiny input ID
#' @param value Initial value
#' @param height The height of the editor in pixels
#' @param inline If TRUE, the editor is displayed in an inline block
#' @param branding If TRUE, give some love to the TINY team
#' @param menubar If TRUE, displays a more advanced menu
#' @param toolbar The buttons to display above the editor, in string format. 
#' @export
#' @rdname htmlInput
htmlInput <- function(inputId, 
                      label = NULL,
                      value = NULL,
                      height = 500,
                      inline = FALSE,
                      branding = FALSE,
                      menubar = FALSE,
                      toolbar = "styleselect | bold italic | numlist bullist | outdent indent | undo redo | insertdatetime",
                      plugins = c("lists","insertdatetime","autoresize"),
                      contextmenu = "",
                      ...){
  
  if(is.na(value))value <- ""
  
  value <- shiny::restoreInput(id = inputId, default = value)
  
  if(endsWith(as.character(height),"px")){
    height <- as.integer(gsub("px","",height))
  }
  
  plugin_string <- ifelse(length(plugins > 0),
                          paste0(
                            "plugins: [",
                            paste0("'",plugins,"'",collapse=','),
                            "],"
                          ), "")
  
  
  shiny::tagList(
    
    htmltools::htmlDependency(
      name = "tinymcebinding", version = "0.1",
      package = "shintocatman",
      src = c(file = "tinymce"),
      script = "shiny-tinymce-bindings.js",
    ),
    
    shiny::tags$script(glue::glue("
                                  tinyMCE.remove('#{{inputId}}');
                                  
                                  tinyMCE.init({selector: '#{{inputId}}',",
                                  "inline: {{tolower(inline)}},",
                                  "branding: {{tolower(branding)}},",
                                  "contextmenu: '{{contextmenu}}',",
                                   plugin_string,
                                  "toolbar: '{{toolbar}}',",
                                  "menubar: {{tolower(menubar)}},",
                                  "height: {{height}}",
                                  #"width: '100%',",
                                  "})", 
                                  .open = "{{", .close = "}}")),
    
    shiny::tags$div(
      # messes with the CSS: we don't seem to need it anyway
      #class = "shiny-input-container", 
      shiny::tags$label(label, class = "control-label", 
                        id = paste0(inputId,"-label"),
                        `for` = inputId),
      shiny::tags$div(style = "width: 100%; padding: 20px; border: 1px solid black;",
                      id = inputId, 
                      class = "shinytinymce", 
                      HTML(value), ...
      )  
    )
    
  )
  
}


#' @rdname htmlInput
#' @export
updatehtmlInput <- function(inputId, value, session = getDefaultReactiveDomain(), asis = FALSE){
  
  if(!asis){
    inputId <- session$ns(inputId)
  }
  
  if(is.na(value))value <- ""
  
  data_list <- list(id = inputId, content = value)
  session$sendCustomMessage(type = "shinyMCE.update", data_list)
}




#' htmlInput dependency
#' @export
useHtmlInput <- function(){
  
  tags$head(
    tags$script(
      src = get_mce_js_path()
    )
  )
  
}


get_mce_js_path <- function(){
  key <- get_mce_apikey()
  paste0("https://cdn.tiny.cloud/1/",key,"/tinymce/5/tinymce.min.js")
}

get_mce_apikey <- function(){
  opt <- getOption("mce_api_key", default = NULL)
  if(is.null(opt)){
    stop("Set options(mce_api_key = '<<TINYMCE API KEY>>')")
  }
  opt
}  





#----- Utils







#=------- Test

test_htmlEditModule <- function(){
  
  devtools::load_all()
  library(softui)
  
  options(mce_api_key = Sys.getenv("TINYMCE_API_KEY"))
  
  ui_module <- function(id){
    ns <- NS(id)
    
    tags$div(
      htmlInput(ns("value"), value = ""),
      actionButton(ns("btn_update"),"Update")  
    )
    
  }
  
  server_module <- function(input, output, session){
    
    observeEvent(input$btn_update, {
      updatehtmlInput("value", value = "HALLO HALLO HALLO")
    })
    
    reactive(input$value)
    
  }
  
  
  ui <- softui::simple_page(
    
    shintocatman::useHtmlInput(),
    
    softui::fluid_row(
      softui::box(width = 6,
                  htmlInput("test", 
                            value = "<b>Hallo!</b>, dit is een <h4>TEST</h4>",
                            height = 300),
                  verbatimTextOutput("txt_out")
      ),
      
      softui::box(
        width = 6,
        uiOutput("ui_test2"),
        actionButton("btn_rerender", "Render again"),
        
        verbatimTextOutput("txt_out2")
      )
    )
    
    
  )
  
  server <- function(input, output, session) {
    
    
    output$txt_out <- renderPrint({
      input$test
    })
    
    output$ui_test2 <- renderUI({
      input$btn_rerender
      ui_module("test")
    })
    
    out2 <- callModule(server_module, "test")
    
    output$txt_out2 <- renderPrint({
      out2()
    })
    
    
  }
  
  shinyApp(ui, server)
  
  
}


