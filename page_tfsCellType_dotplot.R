library(shiny)
library(plotly)

### Prepare components ----
dotPlot.ui <- function(id){
  ns <- NS(id)
  fluidPage(
        plotlyOutput(NS(id, "plot_tfsCellType"), height="900px") %>% withSpinner(color="#0dc5c1")
    )
}

dotPlot.server <- function(input, output, session, filePath) {
  load(paste0(filePath))
  
  ## Can be removed if direcltly loading "p"
  # source("libs/dotheatmap.R")
  # p <- dotheatmap(enrichmentDf=meanExprMat.df,
  #                 var.x="gene", var.y="cellType",
  #                 var.col="expression",
  #                 col.low="lightgrey", col.mid="floralwhite", col.high="red",
  #                 var.size="NES", min.size=.2, max.size=5)
  # # ggplotly(p)
  ###
  
  output$plot_tfsCellType <- renderPlotly(p)
}

### Build page2: 
page_tfsCellType_dotplot <- dotPlot.ui("plot_dotplot") 
page_tfsCellType_dotplot2 <- dotPlot.ui("plot_dotplot2") 
