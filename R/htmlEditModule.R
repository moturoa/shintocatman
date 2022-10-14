

#' HTML edit a field (TinyMCE)
#' @export
#' @importFrom softui action_button
#' @importFrom uuid UUIDgenerate
#' @rdname htmlEditModule
htmlEditModuleUI <- function(id, label = "Edit HTML", ...){
  
  ns <- NS(id)
  
  key <- get_mce_apikey()
  
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
    softui::action_button(ns("btn"), label, ...)
  )
  
}

get_mce_apikey <- function(){
  opt <- getOption("mce_api_key", default = NULL)
  if(is.null(opt)){
    stop("Set options(mce_api_key = '<<TINYMCE API KEY>>')")
  }
  opt
}


#' @rdname htmlEditModule
#' @export
htmlEditModule <- function(input, output, session,
                           title = "", value = reactive("")){
  
  
  last_mc_id <- reactiveVal()
  
  observeEvent(input$btn, {
    
    # Make new ID for editor
    new_id <- uuid::UUIDgenerate()
    last_mc_id(new_id)
    new_id <- session$ns(new_id)
    
    showModal(
      softui::modal(
        title = title,
        close_button = TRUE,
        id_confirm = "btn_confirm_toelichting_edits",
        
        tags$div(style = "width: 100%; height: 500px; padding: 20px; border: 1px solid black;",
                 id = new_id, 
                 class = "shinytinymce", 
                 HTML(value())
        ),
        tags$script(glue("tinymce.init({selector: '#{{new_id}}',",
                         "inline: false,",
                         "branding: false,",
                         "contextmenu: '',",
                         "menubar: false,",
                         "height: 500",
                         "})", 
                         .open = "{{", .close = "}}"))
      )
    )
  })
  
  
  edits <- reactiveVal()
  
  observeEvent(input$btn_confirm_toelichting_edits, {
    
    edits(input[[last_mc_id()]])
  })
  
  
  return(edits) 
}




