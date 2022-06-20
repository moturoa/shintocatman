



#' Shiny color vector editor
#' @param id Shiny input ID
#' @param label Label for button
#' @param icon An icon for the button
#' @param status Button status color in [softui::action_button()]
#' @rdname colorVector
#' @export
colorVectorPickModuleUI <- function(id, label = "Kleuren kiezen", 
                                    icon = softui::bsicon("palette-fill"), 
                                    status = "success"){

  ns <- NS(id)

  softui::action_button(ns("btn"),label, icon = icon, status = status)

}



#' @param input Shiny input, do not set
#' @param input Shiny output, do not set
#' @param input Shiny session, do not set
#' @param n_colors Number of colors - reactive
#' @param current_colors Vector of current colors - reactive
#' @param labels Vector of labels - reactive
#' @param show_order Vector of ordering of the colors (integer) -  reactive
#' @param callback Function to call after the colors have been chosen.
#' @rdname colorVector
#' @export
#' @importFrom colourpicker colourInput
#' @importFrom pals parula viridis
#' @importFrom htmltools tagAppendAttributes
colorVectorPickModule <- function(input, output, session,
                                  n_colors = reactive(NULL),
                                  current_colors = reactive(NULL),
                                  labels = reactive(NULL),
                                  show_order = reactive(NULL),
                                  callback = function(data){}
                                  ) {


  color_ids <- shiny::reactiveVal()
  color_choices <- shiny::reactiveVal()

  shiny::observeEvent(input$btn, {

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

    shiny::showModal(
      softui::modal(
        title = "Kies kleuren",
        size = "m",
        id_confirm = "btn_confirm_colours", 
        confirm_txt = "Opslaan",
        close_txt = "Annuleren",
        
        lapply(1:n_col, function(i){
          my_col_pick(session$ns(ids[i]), labs[i], cur_colors[i])
        }),

        tags$hr(),
        
        softui::fluid_row(
          column(7, 
                 tags$p("Interpoleer de kleuren tussen de eerste en laatste gekozen kleur.")
                 
          ),
          column(5, 
                 softui::action_button(session$ns("btn_make_gradient"),
                     "Maak gradient", status = "secondary",
                     icon = softui::bsicon("brush-fill"))
          )
        ),
        
        tags$hr(),
        
        tags$p("Of kies een palet om kleuren aan te maken:"),
        softui::fluid_row(
          
          column(7, 
                 
            selectInput(session$ns("sel_palette"), "Kies palette",
                        choices = c(
                                   "Parula" = "parula",
                                   "Blauw" = "brewer.blues",
                                   "Blauw - Rood" = "coolwarm",
                                   "Rood - Blauw" = "warmcool",
                                   "Geel - Rood" = "brewer.ylorrd",
                                   "Oranje" = "brewer.oranges",
                                   "Ocean" = "ocean.haline",
                                   "Cubic" = "cubicl",
                                   "Ocean 2" = "ocean.phase",
                                   "Viridis" = "viridis",
                                   "Rainbow" = "kovesi.rainbow",
                                   "Jet" = "jet"
                                   ))
          ),
          column(5, style = "padding-top: 24px;",
                 
              softui::action_button(session$ns("btn_fill_gradient"),
                                    "Vul met palette",
                                    status = "info", 
                                    icon = softui::bsicon("palette-fill"))
          )
          
        )
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

  observeEvent(input$btn_fill_gradient, {
    
    ids <- color_ids()
    n <- length(ids)
    palfun <- getFromNamespace(input$sel_palette, "pals")
    cols <- palfun(n)
    
    for(i in 1:n){
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
  })


  return(color_choices)
}




test_colorVectorPickModule <- function(){
  
  devtools::load_all()
  
  ui <- softui::simple_page(
  
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

