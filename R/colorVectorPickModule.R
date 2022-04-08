



#' Shiny color vector editor
#' @rdname colorVector
#' @export
colorVectorPickModuleUI <- function(id, label = "Kleuren kiezen", 
                                    icon = softui::bsicon("palette-fill"), 
                                    class = "btn-success"){

  ns <- NS(id)

  actionButton(ns("btn"),label, icon = icon, class = class)

}

#' @rdname colorVector
#' @export
#' @importFrom colourpicker colourInput
colorVectorPickModule <- function(input, output, session,
                                  n_colors = reactive(NULL),
                                  current_colors = reactive(NULL),
                                  labels = reactive(NULL),
                                  show_order = reactive(NULL),
                                  callback = function(data){}
                                  ) {


  color_ids <- reactiveVal()
  color_choices <- reactiveVal()

  observeEvent(input$btn, {

    my_col_pick <- function(...){
      colourpicker::colourInput(..., showColour = "both", allowTransparent = TRUE)
    }

    n_col <- n_colors()
    req(n_col > 0)

    cur_colors <- current_colors()
    if(is.null(cur_colors)){
      cur_colors <- rep("#FFF", n_col)
    }

    labs <- labels()
    if(is.null(labs)){
      labs <- as.character(1:n_col)
    }
    
    ord <- show_order()
    if(!is.null(ord)){
      labs <- labs[ord]
      cur_colors <- cur_colors[ord]
    }

    ids <- uuid::UUIDgenerate(n = n_col)
    color_ids(ids)

    showModal(
      modalDialog(
        title = "Kies kleuren",
        size = "m",
        footer = tagList(actionButton(session$ns("btn_confirm_colours"),
                                      "Opslaan",
                                      icon = icon("check"),
                                      class = "btn-success"),
                         htmltools::tagAppendAttributes(modalButton("Annuleren"), class = "btn-danger")
        ),

        lapply(1:n_col, function(i){
          my_col_pick(session$ns(ids[i]), labs[i], cur_colors[i])
        }),

        actionButton(session$ns("btn_make_gradient"),
                     "Gradient vullen",
                     #label_tooltip("Kleuren invullen als gradient van eerste naar laatste kleur."),
                     icon = icon("paint-brush"))
      )
    )

  })

  observeEvent(input$btn_make_gradient, {

    ids <- color_ids()
    req(length(ids)>2)

    n <- length(ids)
    col1 <- input[[ids[1]]]
    col2 <- input[[ids[n]]]
    cols <- colorRampPalette(c(col1,col2))(n)

    for(i in 2:(n-1)){
      colourpicker::updateColourInput(session,
                                      ids[i],
                                      value = cols[i])
    }

  })


  observeEvent(input$btn_confirm_colours, {

    cols <- c()
    ids <- color_ids()
    for(i in 1:length(ids)){
      cols[i] <- input[[ids[i]]]
    }
    color_choices(cols)
    callback()
    removeModal()
  })


  return(color_choices)
}




test_colorVectorPickModule <- function(){
  
  ui <- fluidPage(
  
    #softui::softui_dependencies(),
    colorVectorPickModuleUI("test"),
    tags$hr(),
    verbatimTextOutput("txt_out")
  
  )
  
  server <- function(input, output, session) {
  
    out <- callModule(colorVectorPickModule, "test",
               n_colors = reactive(5),
               current_colors = reactive(pals::parula(5)),
               labels = reactive(c("aap","banaan","boom","tak","modder")),
               show_order = reactive(c(5,1,2,3,4)),
               callback = function(data){print("GELUKT!!")}
               )
  
    output$txt_out <- renderPrint({
      out()
    })
  
  }
  
  
  shinyApp(ui, server)

}

