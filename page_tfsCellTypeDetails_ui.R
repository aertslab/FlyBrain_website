library(shiny)
library(plotly)

### Prepare components ----
plot_tf_details.ui <- function(id){
  inputs <- readRDS("../data/TFsDetail_tfs.Rds")
  inputs <- inputs[c(-1, -2)]
  ns <- NS(id)
  fluidPage(
  fluidRow(selectInput(inputId = NS(id, "tf"), label = "Transcription Factor:", choices=inputs, selected = "ey", selectize = TRUE)), 
  # br(),
  # fluidRow(column(6, plotlyOutput(NS(id, "accessibility_tsne_plot"), height="500px")),
  #          column(6, img(src="img/tsne.png"), width="80%")), # , width="80%" # plotlyOutput(NS(id, "cell_type_tsne"), height="500px")
  br(),
  fluidRow(column(6, plotlyOutput(NS(id, "acc_bar"), height="400px")),
           column(6, plotlyOutput(NS(id, "expr_vs_nes_plot"), height="400px"))),
  
  br(),
  fluidRow(column(6, plotlyOutput(NS(id, "expr_bar"), height="400px")),
           column(6, plotlyOutput(NS(id, "nes_bar"), height="400px")))
  )
}

page_tfsCellTypeDetails <- fluidPage(
          includeMarkdown("md/tfsCellType_details.Rmd"),
          br(),
          plot_tf_details.ui("plots_acc_barplots_nes_expr")
)          
