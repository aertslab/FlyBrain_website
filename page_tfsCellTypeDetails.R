library(shiny)
library(plotly) 
source('libs/plotCont.R')

### ui ----
plot_tf_details.ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(5, 
             selectInput(inputId=NS(id, "tf"), label="Transcription Factor:", choices=NULL, selected=NULL, selectize=TRUE),
             "Plots to show:",
             checkboxInput(inputId=ns("ckNesVsNes"), label="TF expression vs motif", value=TRUE),
             checkboxInput(inputId=NS(id, "ckNesBarplot"), label="TF motif by cell type", value=FALSE),
             checkboxInput(inputId=NS(id, "ckExprBarplot"), label="TF expression by cell type", value=FALSE)
      ),
      column(5, 
             selectInput(inputId=NS(id, "cistromeCellType"), label="Cistrome:", choices=NULL, selected=NULL, selectize=TRUE),
             checkboxInput(inputId=NS(id, "ckTsne"), label="Accessibility (individual cells)", value=TRUE),
             checkboxInput(inputId=NS(id, "ckAccBarplot"), label="Accessibility (cell type)", value=FALSE),
             checkboxInput(inputId=NS(id, "ckMotifs"), label="Motifs", value=TRUE)
      )
    ),
    br(),
    fluidRow(
      column(5, 
             tags$h4("TF expression vs Motif enrichment "), plotlyOutput(NS(id, "expr_vs_nes_plot")),
             tags$h4("TF motif enrichment (per cell type/group)"), plotlyOutput(NS(id, "nes_bar")),
             tags$h4("TF expression (per cell type/group)"), plotlyOutput(NS(id, "expr_bar"))
      ),
      column(5, 
            tags$h4("Cistrome accessibility"),  
             span(textOutput(NS(id, "noCistrome1")), style="color:grey"),
             plotOutput(NS(id, "accessibility_tsne_plot")),
            tags$h4("Cistrome accessibility (per cluster)"), 
             span(textOutput(NS(id, "noCistrome2")), style="color:grey"),
             plotlyOutput(NS(id, "acc_bar")),
            tags$h4("Cell types"), 
             plotOutput(NS(id, "cell_type_tsne"))
      ),
      column(2, 
             tags$h4("Motifs for this TF"), # Motifs supporting this cistrome
             fluidRow(DT::dataTableOutput(NS(id, "tbl_MotifsPerTf"))) # %>% withSpinner(color="#0dc5c1")
      )
    ),
    br(),
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
  plot_tf_details.ui("tfDetails")
)          

### server ----
plot_tf_details.server <- function(input, output, session, dataPath) {
  # Load: 
  meanExprNes <- readRDS(paste0(dataPath,"TFsDetail_meanExprNes.Rds"))
  meanAccMat <- readRDS(paste0(dataPath, "TFsDetail_meanAcc_cistromeByCell.mat.Rds"))
  cistromeByType.df <- readRDS(paste0(dataPath, "TFsDetail_meanAcc_cistromeByType.df.Rds"))
  motifsPerTf <- readRDS("../data/TFsDetail_motifsPerTf_orderedByNes.Rds")
  
  # Aux: 
  load(paste0(dataPath, "drList_adultPupa.RData"))
  drName <- "Adult cells >=900FIP (tSNE, 200topics, 0PCs)" # choose as option?
  drCoords <- drList[[drName]]
  load(paste0(dataPath, "cellData_0.4.1.RData"))
  varName <- "CellType_lvl1"
  load(paste0(dataPath, "colVars_0.4.1.RData"))
  
  # Add list of TFs:
  dataPath <- "../data/"
  tfs <- readRDS(paste0(dataPath,"/TFsDetail_meanExprNes_Tfs.Rds"))
  tfs <- tfs[which(tfs %in% unique(c(gsub("expr_|nes_","", colnames(meanExprNes)),
                                     rownames(meanAccMat), colnames(cistromeByType.df))))]
  updateSelectInput(session, inputId="tf",
                    choices=tfs,
                    selected="ey")

  cistromeCellType <- "" # Initialization
  
  # Start traking events (TF and plots to show):
  observe({
    tf <- input$tf
    
      # cistromeCellType <- input$cistromeCellType

      # if tf changed -> Update cistrome list
      cistromesAvailable <- c(tf, grep(paste0("^",tf, " "), names(cistromeByType.df), value = T, fixed=F))
      if(!cistromeCellType %in% cistromesAvailable)
      {
        cistromeCellType <- cistromesAvailable[1]
        updateSelectInput(session, inputId="cistromeCellType",
                          choices=cistromesAvailable,
                          selected=cistromeCellType)
      }
    
    if(!is.null(tf) & (tf != "")){
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
      
      ## Motif logos ----
      if(input$ckMotifs){
        if(tf %in% names(motifsPerTf))
        {
          isolate({
            output$tbl_MotifsPerTf <- DT::renderDataTable(motifsPerTf[[tf]], 
                                                          # filter="top", 
                                                          escape=FALSE,
                                                          server=TRUE,
                                                          extensions=c("ColReorder", "FixedHeader"), # 
                                                          options=list(
                                                            pageLength = 5
                                                            , colReorder=TRUE
                                                            , dom = 'ritBpl'
                                                            , scrollX=FALSE
                                                            # , scrollY=TRUE # vertical scroll bar within the table
                                                            , fixedHeader = TRUE # header visible while scrolling
                                                          ))
          })
        }else{
          output$tbl_MotifsPerTf <- NULL
        }
      }else{
        output$tbl_MotifsPerTf <- NULL
      }
      
      # ## cell type tsne  #TODO: Static or remove
      # fig_cellType <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2)) +
      #     geom_point(aes(color=cellType), alpha = 1, size=1) +
      #     scale_color_manual(values=colVar, guide = FALSE) + theme_light()
      # output$cell_type_tsne <- renderPlot(fig_cellType)
    }
  })
  
  # Cistrome changes:
  observe({
    cistromeCellType <- input$cistromeCellType
    ## Accessibility tsne ---
    if(input$ckTsne & (cistromeCellType != "")){
      ## TODO: Restore? (with hover for cell type, instead of the two tSNEs?)
      # fig_acc <- ggplot(data=accessibilityMat.df, aes(x = tSNE1, y = tSNE2, color=log(accessibilityMat.df[,tf]*10**5))) + geom_point(alpha = 1/5, size=1) +
      # scale_colour_gradient2(low ="bisque1", high ="red3", midpoint = mean(log(accessibilityMat.df[,tf]*10**5)), space = "Lab", guide = FALSE,aesthetics = "colour") +
      # theme_light()
      # output$accessibility_tsne_plot <- renderPlot(fig_acc)
      if(cistromeCellType %in% rownames(meanAccMat))
      {
        cellVar <- setNames(rep(0,nrow(drCoords)), rownames(drCoords))
        cellVar[colnames(meanAccMat)] <- meanAccMat[cistromeCellType, colnames(meanAccMat)]
        output$noCistrome1 <- NULL
        output$accessibility_tsne_plot <- renderPlot(plotContinuous(drCoords, cellVar[rownames(drCoords)],
                                                                    colorPals=list("high"=grDevices::colorRampPalette(c("pink", "red","darkred"))(10),
                                                                                   "low"=grDevices::colorRampPalette(c("skyblue", "#f0f0f0"))(10)),
                                                                    palBreaks=median(cellVar), cex=0.6, showLegend=F,
                                                                    minMaxVal=0,
                                                                    main=cistromeCellType))
        # if(FALSE){
        #   labsCoords <- t(sapply(split(data.frame(drCoords), as.character(cellData[rownames(drCoords),varName])), colMeans));
        #   for(i in rownames(labsCoords)) text(mean(labsCoords[i,1]), mean(labsCoords[i,2]), i, cex=.5)
        # }
      }else{
        output$noCistrome1 <- renderText("No cistrome available.")
        output$accessibility_tsne_plot <- NULL
      }
    }else{
      output$noCistrome1 <- NULL
      output$accessibility_tsne_plot <- NULL
    }

    ## Accessibility barplot ----
    if(input$ckAccBarplot){
      if(cistromeCellType %in% colnames(cistromeByType.df))
      {
        fig_acc_bar <- plot_ly(cistromeByType.df, x = ~cellType, y = cistromeByType.df[, cistromeCellType],
                               type = 'bar', color = ~cellType, colors = ~cellTypeColor, hoverinfo = "text", text = ~cellType) %>%
          layout(yaxis = list(title = "Cistrome accessibility"), showlegend = FALSE)
        output$noCistrome2 <- NULL
        output$acc_bar <- renderPlotly(fig_acc_bar)
      }else{
        output$noCistrome2 <- renderText("No cistrome available.")
        output$acc_bar <- NULL
      }
    }else{
      output$noCistrome2 <- NULL
      output$acc_bar <- NULL
    }
  })
}

