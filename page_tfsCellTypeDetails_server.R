library(plotly)

plot_acc_tsne.server <- function(input, output, session, dataPath) {
  accessibilityMat <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/accessibilityMat.Rds")

  observe({
    
    tf <- input$tf_accessibility
    accessibilityMat$toPlot <- accessibilityMat[,tf]
     
    fig <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = ~toPlot, colors = 'Reds')
      output$accessibility_tsne_plot <- renderPlotly(fig)
      
  })
}

