library(shiny)
library(plotly) 
source('libs/plotCont.R')

### ui ----
plot_tf_details.ui <- function(id){
  dataPath <- "../data/"
  tfs <- readRDS(paste0(dataPath,"/TFsDetail_meanExprNes_Tfs.Rds"))
  print("done")
  ns <- NS(id)
  fluidPage(
    fluidRow(
        column(2, 
               selectInput(inputId = NS(id, "tf"), label = "Transcription Factor:", choices=tfs, selected = "ey", selectize = TRUE)
        ),
        column(4, 
               "Plots to show:",
               checkboxInput(inputId=NS(id, "ckNesVsNes"), label="TF expression vs motif", value=TRUE),
               checkboxInput(inputId=NS(id, "ckNesBarplot"), label="TF motif by cell type", value=FALSE),
               checkboxInput(inputId=NS(id, "ckExprBarplot"), label="TF expression by cell type", value=FALSE)
        ),
        column(4, 
               checkboxInput(inputId=NS(id, "ckTsne"), label="Accessibility (individual cells)", value=TRUE),
               checkboxInput(inputId=NS(id, "ckAccBarplot"), label="Accessibility (cell type)", value=FALSE)
        )
        ),
    br(),
    fluidRow(
              column(6, 
                     tags$h4("TF expression vs Motif enrichment "), plotlyOutput(NS(id, "expr_vs_nes_plot")),
                     tags$h4("TF motif enrichment (per cell type/group)"), plotlyOutput(NS(id, "nes_bar")),
                     tags$h4("TF expression (per cell type/group)"), plotlyOutput(NS(id, "expr_bar"))
                     ),
              column(6, 
                     tags$h4("Cistrome accessibility"),  plotOutput(NS(id, "accessibility_tsne_plot")),
                     tags$h4("Cistrome accessibility (per cluster)"), plotlyOutput(NS(id, "acc_bar")),
                     tags$h4("Cell types"), plotOutput(NS(id, "cell_type_tsne"))
                     )
              ),
    # br(),
    # ## TODO: Add hide box/panel
    # fluidRow(
    #   column(6, tags$h4("TF motif enrichment (per cell type/group)"), plotlyOutput(NS(id, "nes_bar")))
    #   ,
    #   column(6, tags$h4("Cistrome accessibility (per cluster)"), plotlyOutput(NS(id, "acc_bar")))
    # ),
    # br(),
    # fluidRow(
    #   column(6, tags$h4("TF expression (per cell type/group)"), plotlyOutput(NS(id, "expr_bar")))
    #   ,
    #   column(6, tags$h4("Cell types"), plotOutput(NS(id, "cell_type_tsne")))
    # ),
    "add link to flybase? e.g. https://flybase.org/reports/FBgn0010109"
  )
}

page_tfsCellTypeDetails <- fluidPage(
  includeMarkdown("md/tfsCellType_details.Rmd"),
  br(),
  plot_tf_details.ui("plotTF")
)          

### server ----
plot_tf_details.server <- function(input, output, session, dataPath) {
  # Load: 
  meanExprNes <- readRDS(paste0(dataPath,"TFsDetail_meanExprNes.Rds"))
  meanAccMat <- readRDS(paste0(dataPath, "TFsDetail_meanAcc_cistromeByCell.mat.Rds"))
  cistromeByType.df <- readRDS(paste0(dataPath, "TFsDetail_meanAcc_cistromeByType.df.Rds"))

  # Aux: 
  load(paste0(dataPath, "drList_adultPupa.RData"))
  drName <- "Adult cells >=900FIP (tSNE, 200topics, 0PCs)" # choose as option?
  drCoords <- drList[[drName]]
  load(paste0(dataPath, "cellData_0.4.1.RData"))
  varName <- "CellType_lvl1"
  load(paste0(dataPath, "colVars_0.4.1.RData"))
  
  observe({
    
    tf <- input$tf
    if(tf != ""){
      ## Expression vs NES ----
      if(input$ckNesVsNes){
        fig_nes_expr <- plot_ly(meanExprNes,
                                x=meanExprNes[, paste0("expr_", tf)],
                                y=meanExprNes[, paste0("nes_", tf)],
                                type = 'scatter', mode = 'markers', size = 10,
                                color = ~cellType, colors = ~cellTypeColor, hoverinfo = "text", text = ~cellType) %>%
          layout(showlegend = FALSE) %>%
          layout(yaxis=list(title = "Motif score (highest NES)"), xaxis = list(title = "Expression"))%>% 
          add_segments(x=0, xend=max(meanExprNes[, paste0("expr_", tf)]), y=3, yend=3, 
                       line=list(color = 'lightgrey', width = 2, dash = 'dash'), text="NES threshold") 
        output$expr_vs_nes_plot <- renderPlotly(fig_nes_expr)
      }else{
        output$expr_vs_nes_plot <- NULL
      }
      
      ## NES barplot ----
      if(input$ckNesBarplot){
        ## nes barplot
        fig_nes_bar <- plot_ly(meanExprNes, x = ~cellType, y = meanExprNes[, paste0("nes_", tf)],
                               type = 'bar', color = ~cellType, colors = ~cellTypeColor, hoverinfo = "text", text = ~cellType) %>%
          layout(yaxis = list(title = "highest NES score"), showlegend = FALSE)
        output$nes_bar <- renderPlotly(fig_nes_bar)
      }else{
        output$nes_bar <- NULL
      }
      
      ## Expression barplot ----
      if(input$ckExprBarplot){
        fig_expr_bar <- plot_ly(meanExprNes, x = ~cellType, y = meanExprNes[, paste0("expr_", tf)],
                                type = 'bar', color = ~cellType, colors = ~cellTypeColor, hoverinfo = "text", text = ~cellType) %>%
          layout(yaxis = list(title = "Expression"), showlegend = FALSE)
        output$expr_bar <- renderPlotly(fig_expr_bar)
      }else{
        output$expr_bar <- NULL
      }
      
      ## Accessibility tsne ---
      if(input$ckTsne){
        ## TODO: Restore? (with hover for cell type, instead of the two tSNEs?)
        # fig_acc <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2, color=log(accessibilityMat.df[,tf]*10**5))) + geom_point(alpha = 1/5, size=1) + 
        # scale_colour_gradient2(low ="bisque1", high ="red3", midpoint = mean(log(accessibilityMat.df[,tf]*10**5)), space = "Lab", guide = FALSE,aesthetics = "colour") +
        # theme_light()
        # output$accessibility_tsne_plot <- renderPlot(fig_acc)
        if(tf %in% rownames(meanAccMat))
        {
          cellVar <- setNames(rep(0,nrow(drCoords)), rownames(drCoords))
          cellVar[colnames(meanAccMat)] <- meanAccMat[tf, colnames(meanAccMat)]
          output$accessibility_tsne_plot <- renderPlot(plotContinuous(drCoords, cellVar[rownames(drCoords)],
                                                                      colorPals=list("high"=grDevices::colorRampPalette(c("pink", "red","darkred"))(10),
                                                                                     "low"=grDevices::colorRampPalette(c("skyblue", "#f0f0f0"))(10)),
                                                                      palBreaks=median(cellVar), cex=0.6, showLegend=F,
                                                                      minMaxVal=0,
                                                                      main=tf))
          if(FALSE){
            labsCoords <- t(sapply(split(data.frame(drCoords), as.character(cellData[rownames(drCoords),varName])), colMeans));
            for(i in rownames(labsCoords)) text(mean(labsCoords[i,1]), mean(labsCoords[i,2]), i, cex=.5)
          }
        }else{
          output$accessibility_tsne_plot <- NULL
        }
      }else{
        output$accessibility_tsne_plot <- NULL
      }
      
      ## Accessibility barplot ----
      if(input$ckAccBarplot){
        if(tf %in% colnames(cistromeByType.df))
        {
          fig_acc_bar <- plot_ly(cistromeByType.df, x = ~cellType, y = cistromeByType.df[, tf],
                                 type = 'bar', color = ~cellType, colors = ~cellTypeColor, hoverinfo = "text", text = ~cellType) %>%
            layout(yaxis = list(title = "Cistrome accessibility"), showlegend = FALSE)
          output$acc_bar <- renderPlotly(fig_acc_bar)
        }else{
          output$acc_bar <- NULL
        }
      }else{
        output$acc_bar <- NULL
      }
      
      # ## cell type tsne  #TODO: Static or remove
      # fig_cellType <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2)) +
      #     geom_point(aes(color=cellType), alpha = 1, size=1) +
      #     scale_color_manual(values=colVar, guide = FALSE) + theme_light()
      # output$cell_type_tsne <- renderPlot(fig_cellType)
    }
  })
}

