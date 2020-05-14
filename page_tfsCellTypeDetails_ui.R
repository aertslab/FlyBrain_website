library(shiny)
library(plotly)

### Prepare components ----
plot_tf_details.ui <- function(id){
  inputs <- readRDS("../data/TFsDetail_tfs.Rds")
  inputs <- inputs[c(-1, -2)]
  ns <- NS(id)
  fluidPage(
  fluidRow(selectInput(inputId = NS(id, "tf"), label = "Transcription Factor:", choices=inputs, selected = "ey", selectize = TRUE)), 
  br(),
  fluidRow(column(6, plotOutput(NS(id, "accessibility_tsne_plot"))),
          column(6, plotOutput(NS(id, "cell_type_tsne")))),
  br(),
  fluidRow(column(6, plotlyOutput(NS(id, "acc_bar"))),
           column(6, plotlyOutput(NS(id, "expr_vs_nes_plot")))),
  
  br(),
  fluidRow(column(6, plotlyOutput(NS(id, "expr_bar"))),
           column(6, plotlyOutput(NS(id, "nes_bar"))))
  ,
  "add link to flybase? e.g. https://flybase.org/reports/FBgn0010109"
  )
}

page_tfsCellTypeDetails <- fluidPage(
          includeMarkdown("md/tfsCellType_details.Rmd"),
          br(),
          plot_tf_details.ui("plotTF")
)          
