library(plotly)

plot_tf_details.server <- function(input, output, session, dataPath) {
  load("../data/TFsDetail_cellTypeColVar.RData")
  accessibilityMat <- readRDS("../data/TFsDetail_accessibilityMat.Rds")
  meanExprNesMat.df <- readRDS("../data/TFsDetail_meanExprNesMat.df.Rds")
  meanAccPerTypeMat.df <- readRDS("../data/TFsDetail_meanAccPerTypeMat.df.Rds")
  
  observe({
    
    tf <- input$tf
    if(tf != ""){
      ## accessibility tsne
      fig_acc <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = accessibilityMat[,tf], colors = 'Reds', marker = list(size=3))
      output$accessibility_tsne_plot <- renderPlotly(fig_acc)
      
      ##cell type tsne
      # fig_cellType <- plot_ly(accessibilityMat, x = ~tSNE1, y = ~tSNE2, type = 'scatter', mode = 'markers', color = ~cellType, colors = colVar, marker = list(size=3), 
      #                         hoverinfo = 'text', text= ~paste("Cluster: ", cellType), showlegend = FALSE)
      # output$cell_type_tsne <- renderPlotly(fig_cellType)
      
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

