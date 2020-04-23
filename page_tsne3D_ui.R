library(shiny)
library(plotly)
### Prepare components ----
plot3d.ui <- function(id){
  ns <- NS(id)
  fluidPage(
        # absolutePanel( 
        #   id="plot_3d_controls", 
        #   class = "panel panel-default", fixed = TRUE, 
        #   draggable = FALSE, # messy with the 3D plot ... :(
        #   top = 60, left = "auto", right = 20, bottom = "auto", # width = 330, height = "auto",
        #   # HTML('<button data-toggle="collapse" data-target="#demo">Collapsible</button>'),
        #     # tags$div(id = 'demo',  class="collapse", checkboxInput('input_draw_point', 'Draw point', FALSE ), verbatimTextOutput('summary'))

        selectInput(inputId =  NS(id, "plot3d_dataset"),
                    label = "Dataset:",
                    choices=c("Larva to Pupa12h"),
                    selected="Larva to Pupa12h"),
        selectInput(inputId =  NS(id, "plot3d_pntCol"),
                    label = "Color:",
                    choices=c("clusters","stage"),
                    selected="clusters"),
        
        sliderInput(inputId = NS(id, "plot3d_perCells"),
                    label = "Subsample cells (% to show):",
                    min = 0.1,
                    max = 100,
                    value = 10),
        
        sliderInput(inputId = NS(id, "plot3d_pntSize"),
                    label = "Point size:",
                    min = 0.01,
                    max = 10,
                    value = 3),
        plotlyOutput(NS(id, "plot_3d"), height="900px")
    )
}

### Build page ----
page_tsne3D <- plot3d.ui("plot_tsne3d")




