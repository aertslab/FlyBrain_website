library(plotly)

plot_tf_details.server <- function(input, output, session, dataPath) {
  load("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/cellTypeColVar.RData")
  accessibilityMat.df <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/accessibilityMat.df.Rds")
  meanExprNesMat.df <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/meanExprNesMat.df.Rds")
  meanAccPerTypeMat.df <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/meanAccPerTypeMat.df.Rds")
  
  observe({
    
    tf <- input$tf
    if(tf != ""){
      ## accessibility tsne
      fig_acc <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2, color=accessibilityMat[,tf])) + geom_point(alpha = 1/5, size=1) + 
      scale_colour_gradient2(low ="pink", high ="red3", midpoint = 0, space = "Lab", guide = FALSE,aesthetics = "colour") +
      theme_light()
      output$accessibility_tsne_plot <- renderPlot(fig_acc)
      #fig_acc <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = accessibilityMat[,tf], colors = 'Reds', marker = list(size=3))
      #output$accessibility_tsne_plot <- renderPlotly(fig_acc)
      
      ##cell type tsne
      fig_cellType <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2)) + geom_point(aes(color=cellType), alpha = 1, size=1) +
          scale_color_manual(values=colVar, guide = FALSE) + theme_light() 
      output$cell_type_tsne <- renderPlot(fig_cellType)
      #fig_cellType <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = ~cellType, colors = colVar, marker = list(size=3), 
      #                         hoverinfo = 'text', text= ~paste("Cluster: ", cellType), showlegend = FALSE)
      #output$cell_type_tsne <- renderPlotly(fig_cellType)
      
      ## expression vs nes dotplot
      fig_nes_expr <- plot_ly(meanExprNesMat.df, x = meanExprNesMat.df[, paste0("expr_", tf)], y = meanExprNesMat.df[, paste0("nes_", tf)], 
                              type = 'scatter', mode = 'markers', color = ~cellType, colors = ~colors)
      fig_nes_expr <- fig_nes_expr %>% layout(yaxis = list(title = "highest NES score"), xaxis = list(title = "Expression"))
      
      output$expr_vs_nes_plot <- renderPlotly(fig_nes_expr)
      
      ## expression barplot
      fig_expr_bar <- plot_ly(meanExprNesMat.df, x = ~cellType, y = meanExprNesMat.df[, paste0("expr_", tf)], 
                              type = 'bar', color = ~cellType, colors = ~colors)
      fig_expr_bar <- fig_expr_bar %>% layout(yaxis = list(title = "Expression"), showlegend = FALSE)
      output$expr_bar <- renderPlotly(fig_expr_bar)
      
      ## nes barplot
      fig_nes_bar <- plot_ly(meanExprNesMat.df, x = ~cellType, y = meanExprNesMat.df[, paste0("nes_", tf)], 
                             type = 'bar', color = ~cellType, colors = ~colors)
      fig_nes_bar <- fig_nes_bar %>% layout(yaxis = list(title = "highest NES score"), showlegend = FALSE)
      output$nes_bar <- renderPlotly(fig_nes_bar)
      
      ## accessibility barplot
      fig_acc_bar <- plot_ly(meanAccPerTypeMat.df, x = ~cellType, y = meanAccPerTypeMat.df[, tf], 
                             type = 'bar', color = ~cellType, colors = ~colors)
      fig_acc_bar <- fig_acc_bar %>% layout(yaxis = list(title = "Cistrome accessibility"), showlegend = FALSE)
      output$acc_bar <- renderPlotly(fig_acc_bar) 
    }
  })
}

