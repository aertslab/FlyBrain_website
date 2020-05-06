library(shiny)
library(plotly)

### Prepare components ----
plot_tf_details.ui <- function(id){
  inputs <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/tfs.Rds")
  inputs <- inputs[c(-1, -2)]
  ns <- NS(id)
  fluidPage(
  fluidRow(selectInput(inputId = NS(id, "tf"), label = "Transcription Factor:", choices=inputs, selected = "ey", selectize = TRUE)),
  br(),
  fluidRow(column(6, plotlyOutput(NS(id, "accessibility_tsne_plot"), height="400px")),
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
