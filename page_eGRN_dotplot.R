library(shiny)
library(plotly)

### Prepare components ----
dotPlot.ui <- function(id){
  ns <- NS(id)
  settingsNameChoices <- c(
    "Open chrom."="Motifs_posCorr",
    "Close chrom."="Motifs_negCorr"
    # "DARs up - ChIP"="ChIP_up",
    # "DARs up - motifs (extended annot)"="MotifsExt_up",
    # "DARs up - motifs (extended annot & extremeRocTr)"="MotifsExt_up_extremeRocTrh",
    # "DARs down - motifs"="Motifs_dw",
    # "DARs down - ChIP"="ChIP_dw",
    # "DARs down - motifs (extended annot)"="MotifsExt_dw",
    # "DARs down - motifs (extended annot & extremeRocTr)"="MotifsExt_dw_extremeRocTrh",

    # "* Peaks - motifs"="Motifs_Peaks",
    # "* Peaks - ChIP"="ChIP_Peaks",
    # 
    # "* Topics - motifs"="Motifs_Topics",
    # "* Topics - ChIP"="ChIP_Topics"
  ) # names in the dotplotList object
  
  fluidPage(
    fluidRow(
      column(4, #style="background-color:#b0c6fb",
             selectInput(inputId=NS(id, "settingsName"), 
                         label = "TF enrichment:", 
                         choices=settingsNameChoices,
                         selected = "Motifs_posCorr", 
                         selectize = TRUE),
             checkboxInput(inputId=NS(id, "transposeDotplot"), label="Transpose", value=FALSE)
      ),
      # column(2, #style="background-color:#ffa153",
      #        "Cell types to show:",
      #        checkboxInput(inputId=NS(id, "ckCellTypes"), label="Cell type", value=TRUE),
      #        checkboxInput(inputId=NS(id, "ckMajor"), label="Major types", value=FALSE),
      #        checkboxInput(inputId=NS(id, "ckMerged"), label="Merged types", value=FALSE)
      # ),
      column(4, #style="background-color:#b1f6c6",
             textAreaInput(ns("txt_tfOrd"), "TF order"), #, height = "300px"
             actionButton(ns("bnt_updateTfs"), "Update"),
             actionButton(ns("bnt_resetTFs"), "Reset")
      )),
    fluidRow(
      plotlyOutput(NS(id, "plot_tfsDotplot"), height="900px") %>% withSpinner(color="#0dc5c1")
    )
  )
}

dotPlot.server <- function(input, output, session, dataPath) {
  source("libs/dotheatmap.R")
  # Load files:
  load(paste0(dataPath,"/dotplotsList.RData"))
  #TODO: allow changing TF order
  load(paste0(dataPath,"/dotplots_tfOrder.RData"))
  
  tfOrder <- tfOrderList[["Motifs_posCorr"]]
  updateTextInput(session, "txt_tfOrd", value=paste(rev(tfOrder), collapse="\n"))
  
  observeEvent(input$bnt_resetTFs, {
    tfOrder <- tfOrderList[[input$settingsName]]
    updateTextInput(session, "txt_tfOrd", value=paste(rev(tfOrder), collapse="\n"))
  })

  observe({
    # Get settings
    settingsName <- input$settingsName
    # cellTypeTypes <- c("Cell type", "Major types", "Merged")[which(c(input$ckCellTypes, input$ckMajor, input$ckMerged))]
    transposeDotplot <- !input$transposeDotplot 
    
    justToTrigger <- input$bnt_updateTfs
    isolate({
      if(nchar(input$txt_tfOrd)>0){
        tfOrder <- rev(strsplit(input$txt_tfOrd, "\n")[[1]])
        tfOrder <- trimws(tfOrder) 
        tfOrder <- unique(tfOrder)
      }
    })
    
    # Plot
    # if((length(cellTypeTypes)>0) && (length(tfOrder)>0))
    # {
    dotPlot.df <- dotplotsList[[settingsName]]
    # dotPlot.df <- dotPlot.df[dotPlot.df$ct %in% cellTypeTypes, ]
    dotPlot.df <- dotPlot.df[dotPlot.df$TF %in% tfOrder,]
    dotPlot.df$TF <- factor(dotPlot.df$TF, levels=intersect(tfOrder, unique(dotPlot.df$TF)))
    if(nrow(dotPlot.df)>0)
    {  
      varXY <- c(X="TFwAnnot", Y="cellType"); if(transposeDotplot) varXY <- c(X="cellType", Y="TFwAnnot")
      dir <- "up"; if(grepl("neg", settingsName)) dir="down"
      hmCols <- rbind(up=c(low="lightgrey", mid="seashell2", high="red"),
                      down=c("dodgerblue", "lightgrey", "red"))
    
      # Border doesnt work in website (all dots in same color -.- )
      p <- dotheatmap(enrichmentDf=dotPlot.df,
                              # top = 5, #order.by = "Binom_Fold_Enrichment",
                              var.x=varXY["X"], var.y=varXY["Y"], var.border="eRegulon",
                              var.col="expression",
                              col.low=hmCols[dir,"low"], col.mid=hmCols[dir,"mid"], col.high=hmCols[dir,"high"], midpoint=0.40,
                              var.size="NES", min.size=1, max.size=5, sizeRange=c(3,max(dotPlot.df$NES))) +
        theme(legend.position = "none") +
        aes(text = paste(cellType, "\nTF:", TF, 
                         "\nMotif NES: ",NES, 
                         "\nExpression: ",expression)) 
      output$plot_tfsDotplot <- renderPlotly(ggplotly(p, tooltip="text")) 
    }
  })
}

### Build page: 
page_eGRN_dotplot <- dotPlot.ui("plot_dotplots")
