

library(shiny)
library(shintocatman)
library(softui)
library(knitr)
library(shinipsum)

options(mce_api_key = 'v57hwcvdkp34j4gn9qh58e55p8y7c5bx1lgzkskcc0ug414z')

ui <- softui::simple_page(
  
  shintocatman::useHtmlInput(),
  
  softui::fluid_row(
    softui::box(width=4,
                
          htmlInput("html1",
                    toolbar = "styleselect | charmap | fullscreen | preview",
                    plugins = c("autoresize","charmap","fullscreen","preview"),
                    menubar = FALSE,
                    value = shinipsum::random_text(nwords = 100)
                    ),
      
    ),
    softui::box(width=4,
                
                # zonder autoresize
                htmlInput("html2",
                          toolbar = "styleselect | bold italic | numlist bullist | outdent indent | undo redo | insertdatetime",
                          plugins = c("lists","insertdatetime"),
                          menubar = FALSE,
                          height = "1000px",
                          value = shinipsum::random_text(nwords = 200)
                )
                
    ),
    softui::box(width=4,
                
                # html table, met preview
                htmlInput("html3",
                          toolbar = "styleselect | bold italic | numlist bullist | outdent indent | undo redo | preview",
                          plugins = c("lists","autoresize","preview"),
                          menubar = FALSE,
                          value = knitr::kable(shinipsum::random_table(10,10))
                )
                
    )
  )
  
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
