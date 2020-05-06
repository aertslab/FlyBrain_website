library(plotly)

plot_tf_details.server <- function(input, output, session, dataPath) {
  accessibilityMat <- readRDS("../data/TFsDetail_accMat.Rds")
  load("../data/colVars_0.3.2.RData")
  meanExprMat <- readRDS("../data/TFsDetail_meanExprMat.Rds")
  nesMat <- readRDS("../data/TFsDetail_nesMat.Rds")

  observe({
    
    tf <- input$tf
    
    ## accessibility tsne
    accessibilityMat$toPlot <- accessibilityMat[,tf]
    fig_acc <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = ~toPlot, colors = 'Reds', marker = list(size=3))
    output$accessibility_tsne_plot <- renderPlotly(fig_acc)
      
    ## expression vs nes dotplot
    toPlot <- data.frame(t(rbind(meanExprMat[tf,], nesMat[tf,])))
    colnames(toPlot) <- c("Expression", "NES")
    toPlot$cellType <- rownames(toPlot)
    colors <- colVars$Annot_cellType[toPlot$cellType]
    colors <- colors[order(as.character(names(colors)))]
    toPlot$colors <- colors
    fig_nes_expr <- plot_ly(toPlot, x = ~Expression, y = ~NES, type = 'scatter', mode = 'markers', color = ~cellType, colors = ~colors)
    output$expr_vs_nes_plot <- renderPlotly(fig_nes_expr)
    
    ## expression barplot
    fig_expr_bar <- plot_ly(toPlot, x = ~cellType, y = ~Expression, type = 'bar', color = ~cellType, colors = ~colors)
    output$expr_bar <- renderPlotly(fig_expr_bar)
    
    ## nes barplot
    fig_nes_bar <- plot_ly(toPlot, x = ~cellType, y = ~NES, type = 'bar', color = ~cellType, colors = ~colors)
    output$nes_bar <- renderPlotly(fig_nes_bar)
    
    
  })
}

