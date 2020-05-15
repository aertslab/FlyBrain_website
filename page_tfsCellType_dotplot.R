library(shiny)
library(plotly)

### Prepare components ----
dotPlot.ui <- function(id){
  ns <- NS(id)
  tabsetPanel(type = "tabs",
              id = "dotplots",

              tabPanel("Cell types with high confidence TF annotation",
                       value = "dotplot1",
                       plotlyOutput(NS(id, "plot_tfsCellType_hc"), height="900px") %>% withSpinner(color="#0dc5c1")
              ),
              
              tabPanel("Branches with high confidence TF annotation",
                       value = "dotplot3",
                       plotlyOutput(NS(id, "plot_tfsBranches_hc"), height="900px") %>% withSpinner(color="#0dc5c1")
              ),
              
              tabPanel("Cell types with any TF annotation",
                       value = "dotplot2",
                       plotlyOutput(NS(id, "plot_tfsCellType_both"), height="900px") %>% withSpinner(color="#0dc5c1"),
                       br(),
                       "Note for the any Annot annotation: TFs appear twice: Those without sufix are the same as in the other tab ('high conf': direc & ort), the ones with '[LC]' sufix also use 'motif similarity' for Motif-TF annotation."
              ),
              
              tabPanel("Branches with any TF annotation",
                       value = "dotplot4",
                       plotlyOutput(NS(id, "plot_tfsBranches_both"), height="900px") %>% withSpinner(color="#0dc5c1"),
                       "Note for the any Annot annotation: TFs appear twice: Those without sufix are the same as in the other tab ('high conf': direc & ort), the ones with '[LC]' sufix also use 'motif similarity' for Motif-TF annotation."
              )
          )
}

dotPlot.server <- function(input, output, session, filePath) {
  source("libs/dotheatmap.R")
  load("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/data/dotPlot_highConfAnnot_UPdars.p.RData")  
  meanExprMat.df.highConf <- p$data
  load("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/data/dotPlot_bothAnnots.p.RData") 
  meanExprMat.df.both <- p$data
  subsetCellTypes_hc <- meanExprMat.df.highConf[- c(grep("merged", meanExprMat.df.highConf$cellType), grep("Branch", meanExprMat.df.highConf$cellType)),]
  subsetBranches_hc <- meanExprMat.df.highConf[c(grep("merged", meanExprMat.df.highConf$cellType), grep("Branch", meanExprMat.df.highConf$cellType)),]
  subsetCellTypes_both <- meanExprMat.df.both[- c(grep("merged", meanExprMat.df.both$cellType), grep("Branch", meanExprMat.df.both$cellType)),]
  subsetBranches_both <- meanExprMat.df.both[c(grep("merged", meanExprMat.df.both$cellType), grep("Branch", meanExprMat.df.both$cellType)),]
  p_celltypes_hc <- dotheatmap(enrichmentDf=subsetCellTypes_hc,
                   var.x="gene", var.y="cellType",
                   var.col="expression",
                   col.low="lightgrey", col.mid="floralwhite", col.high="red",
                   var.size="NES", min.size=.2, max.size=5)
  ggplotly(p_celltypes_hc)
  p_branches_hc <- dotheatmap(enrichmentDf=subsetBranches_hc,
                            var.x="gene", var.y="cellType",
                            var.col="expression",
                            col.low="lightgrey", col.mid="floralwhite", col.high="red",
                            var.size="NES", min.size=.2, max.size=5)
  ggplotly(p_branches_hc)
  p_celltypes_both <- dotheatmap(enrichmentDf=subsetCellTypes_both,
                               var.x="gene", var.y="cellType",
                               var.col="expression",
                               col.low="lightgrey", col.mid="floralwhite", col.high="red",
                               var.size="NES", min.size=.2, max.size=5)
  ggplotly(p_celltypes_both)
  p_branches_both <- dotheatmap(enrichmentDf=subsetBranches_both,
                              var.x="gene", var.y="cellType",
                              var.col="expression",
                              col.low="lightgrey", col.mid="floralwhite", col.high="red",
                              var.size="NES", min.size=.2, max.size=5)
  ggplotly(p_branches_both)
  ###
  
  output$plot_tfsCellType_hc <- renderPlotly(p_celltypes_hc)
  output$plot_tfsBranches_hc <- renderPlotly(p_branches_hc)
  output$plot_tfsCellType_both <- renderPlotly(p_celltypes_both)
  output$plot_tfsBranches_both <- renderPlotly(p_branches_both)
}

### Build page2: 
page_tfsCellType_dotplot <- dotPlot.ui("plot_dotplots") 

