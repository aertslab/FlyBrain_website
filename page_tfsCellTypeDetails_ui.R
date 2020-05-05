library(shiny)
library(plotly)

### Prepare components ----
plot_acc_tsne.ui <- function(id){
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(3, selectInput(inputId =  NS(id, "tf_accessibility"),
                          label = "Transcription Factor:",
                          choices=c("ey", "repo", "TfAP-2"),
                          selected="ey"))),
  br(),
  fluidRow(column(12, plotlyOutput(NS(id, "accessibility_tsne_plot"), height="400px")))
  )
}

page_tfsCellTypeDetails <- fluidPage(
  
          includeMarkdown("md/tfsCellType_details.Rmd"),
          br(),
          img(src="img/TF_details.png", width="80%"),
          br(),
          br(),
          img(src="img/expressionVSnes.png", width="80%"),
          br(),
          br(),
          img(src="img/barplots.png", width="80%"),
          br(),
          br(),
          img(src="img/acc_tsnes.png", width="80%"),
          br(),
          br(),
          plot_acc_tsne.ui("plot_cistrome_accessibility")
)          
