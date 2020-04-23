library(plotly)

# TODO: Remove, just as example
dotPlot.server <- function(input, output, session, filePath) {
  load(paste0(filePath))
  source("libs/dotheatmap.R")
  p <- dotheatmap(enrichmentDf=meanExprMat.df,
                  var.x="cellType", var.y="gene",
                  var.col="expression",
                  col.low="lightgrey", col.mid="floralwhite", col.high="red",
                  var.size="NES", min.size=.2, max.size=5)
  # ggplotly(p)
  output$plot_tfsCellType <- renderPlotly(p)
}