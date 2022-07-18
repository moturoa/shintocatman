



#' Shiny color vector editor
#' @param id Shiny input ID
#' @param label Label for button
#' @param icon An icon for the button
#' @param status Button status color in [softui::action_button()]
#' @rdname colorVector
#' @export
colorVectorPickModuleUI <- function(id){

  ns <- NS(id)

  uiOutput(ns("ui_color_editor"))

}



#' @param input Shiny input, do not set
#' @param input Shiny output, do not set
#' @param input Shiny session, do not set
#' @param n_colors Number of colors - reactive
#' @param current_colors Vector of current colors - reactive
#' @param labels Vector of labels - reactive
#' @param show_order Vector of ordering of the colors (integer) -  reactive
#' @rdname colorVector
#' @export
#' @importFrom colourpicker colourInput
#' @importFrom pals parula viridis
#' @importFrom htmltools tagAppendAttributes
#' @importFrom utils getFromNamespace
#' 
colorVectorPickModule <- function(input, output, session,
                                  n_colors = reactive(NULL),
                                  current_colors = reactive(NULL),
                                  labels = reactive(NULL),
                                  show_order = reactive(NULL),
                                  palettes_from_pals = c(
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
                                  )) {

  # Util (intern)
  my_col_pick <- function(...){
    colourpicker::colourInput(..., showColour = "both", allowTransparent = TRUE)
  }

  color_ids <- shiny::reactiveVal()

  
  output$ui_color_editor <- shiny::renderUI({
    
    n_col <- n_colors()
    req(n_col > 0)
    
    # Input colors to edit; if none given fill all with white
    cur_colors <- current_colors()
    if(is.null(cur_colors)){
      cur_colors <- rep("#FFF", n_col)
    }
    
    # Labels for colors. If not given, make 1:n
    labs <- labels()
    if(is.null(labs)){
      labs <- as.character(1:n_col)
    }
    
    # Order of the colors (this can be input with a separate reactive vector)
    ord <- show_order()
    if(!is.null(ord)){
      labs <- labs[ord]
      cur_colors <- cur_colors[ord]
    }
    
    # IDs for the color input fields
    ids <- uuid::UUIDgenerate(n = n_col)
    color_ids(ids)
    
    
    shiny::tagList(
      
      # UI for color editors
      lapply(1:n_col, function(i){
        my_col_pick(session$ns(ids[i]), labs[i], cur_colors[i])
      }),
      
      shiny::tags$hr(),
      
      # interpolate colors
      softui::fluid_row(
        shiny::column(7, 
               shiny::tags$p("Interpoleer de kleuren tussen de eerste en laatste gekozen kleur.")
               
        ),
        shiny::column(5, 
               softui::action_button(session$ns("btn_make_gradient"),
                                     "Maak gradient", status = "secondary",
                                     icon = softui::bsicon("brush-fill"))
        )
      ),
      
      shiny::tags$hr(),
      
      # choose a color palette
      shiny::tags$p("Of kies een palet om kleuren aan te maken:"),
      softui::fluid_row(
        
        shiny::column(7, 
               
          shiny::selectInput(session$ns("sel_palette"), "Kies palette",
               choices = palettes_from_pals)
        ),
        shiny::column(5, style = "padding-top: 24px;",
               
               softui::action_button(session$ns("btn_fill_gradient"),
                                     "Vul met palette",
                                     status = "info", 
                                     icon = softui::bsicon("palette-fill"))
        )
        
      )
    )
    
    
  })

  # make gradient from color 1 to color n
  shiny::observeEvent(input$btn_make_gradient, {

    ids <- color_ids()
    req(length(ids)>2)

    n <- length(ids)
    col1 <- input[[ids[1]]]
    col2 <- input[[ids[n]]]
    cols <- grDevices::colorRampPalette(c(col1,col2))(n)

    for(i in 2:(n-1)){
      colourpicker::updateColourInput(session,
                                      ids[i],
                                      value = cols[i])
    }

  })

  # fill all values with values from a color generating function,
  # from the pals package
  shiny::observeEvent(input$btn_fill_gradient, {
    
    ids <- color_ids()
    n <- length(ids)
    
    # ONLY from pals package
    palfun <- utils::getFromNamespace(input$sel_palette, "pals")
    cols <- palfun(n)
    
    for(i in 1:n){
      colourpicker::updateColourInput(session,
                                      ids[i],
                                      value = cols[i])
    }
    
  })

  out <- shiny::reactive({

    cols <- c()
    ids <- color_ids()
    for(i in 1:length(ids)){
      cols[i] <- input[[ids[i]]]
    }
    cols
    
  })


  return(out)
}




test_colorVectorPickModule <- function(){
  
  devtools::load_all()
  
  ui <- softui::simple_page(
    
    softui::fluid_row(
  
      softui::box(title = "Colors", width = 4,
        colorVectorPickModuleUI("test"),
        tags$hr(),
        verbatimTextOutput("txt_out")
      ),
      softui::box(title = "Colors", width = 4,
                  softui::action_button("btn1", "Edit colors!", status = "success",
                                        icon = bsicon("palette")),
                  tags$hr(),
                  verbatimTextOutput("txt_out2")
      )
    )
  )
  
  server <- function(input, output, session) {
  
    out <- callModule(colorVectorPickModule, "test",
               n_colors = reactive(5),
               current_colors = reactive(pals::parula(5)),
               labels = reactive(c("aap","banaan","boom","tak","modder")),
               show_order = reactive(c(5,1,2,3,4))
               )
  
    output$txt_out <- renderPrint({
      out()
    })
    
    out2 <- softui::modalize(trigger_open = reactive(input$btn1),
                                    ui_module = colorVectorPickModuleUI,
                                    server_module = colorVectorPickModule,
                                    server_pars = list(
                                      n_colors = reactive(5),
                                      current_colors = reactive(pals::parula(5)),
                                      labels = reactive(c("aap","banaan","boom","tak","modder")),
                                      show_order = reactive(c(5,1,2,3,4))
                                    )
    )    
  
    output$txt_out2 <- renderPrint({
      out2()
    })
    
  }
  
  
  shinyApp(ui, server)

}

