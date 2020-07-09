library(shiny)
library(plotly)
library(ComplexHeatmap)

### Prepare components ----
topics.ui <- function(id){
  
  ns <- NS(id)
  
  settingsNameChoices <- c(
    # "Region types"="topicRegionTypes_heatmap",
    "Cell types (RSS; filtered)"="rss",
    "Cell types (RSS; all)"="rss_all"
  )

  fluidPage(
    fluidRow(plotOutput(NS(id, "topicRegionTypes_heatmap"))),
    fluidRow(
      column(8, #style="background-color:#b0c6fb",
             selectInput(inputId=NS(id, "plotTypeSelected"),
                         label = "Plot topics:",
                         choices=settingsNameChoices,
                         selected = "Cell types (RSS; filtered)",
                         selectize = TRUE)
             # checkboxInput(inputId=NS(id, "transposePlot"), label="Transpose", value=FALSE)
      )),
    fluidRow(plotlyOutput(NS(id, "plot_topicsDotplot"), height="900px") %>% withSpinner(color="#0dc5c1")),
    fluidRow(plotOutput(NS(id, "topicCellTypes_heatmap"), height="900px"))
  )
}

topics.server <- function(input, output, session, dataPath) {
  # Load files:
  topicRegionTypesHeatmap <- readRDS(paste0(dataPath,"/TopicRegionTypes_hm.Rds"))
  topicCellTypesHeatmap <- readRDS(paste0(dataPath,"/TopicCellTypes_hm.Rds"))
  rss.df <- readRDS(paste0(dataPath,"/TopicRSS.Rds"))
  
  observe({
    plotTypeSelected <- input$plotTypeSelected
    if(!is.null(plotTypeSelected) & (plotTypeSelected != ""))
    {
      # transposeDotplot <- input$transposePlot
      # varXY <- c(X="TF", Y="cellType"); if(transposeDotplot) varXY <- c(X="cellType", Y="TF")
      
      # if(plotTypeSelected=="topicRegionTypes_heatmap")
      # {
        # output$plot_topicsDotplot <- NULL
        output$topicRegionTypes_heatmap <- renderPlot(draw(topicRegionTypesHeatmap, heatmap_legend_side = "bottom"))
      # }
      
      if(plotTypeSelected=="rss_all")
      {
        p <- dotheatmap(rss.df,
                        var.x="Topic", var.y="cellType",
                        var.size="Z", min.size=.5, max.size=5,
                        var.col="RSS", col.low="grey90", col.mid="darkolivegreen3", col.high="darkgreen")
        # output$topicRegionTypes_heatmap <- NULL
        output$plot_topicsDotplot <- renderPlotly(ggplotly(p))# , tooltip="text"
      }
      
      if(plotTypeSelected=="rss")
      {
        p <- dotheatmap(rss.df[which(rss.df$Z>=5),],
                        var.x="Topic", var.y="cellType",
                        var.size="Z", min.size=.5, max.size=5,
                        var.col="RSS", col.low="grey90", col.mid="darkolivegreen3", col.high="darkgreen")
        # output$topicRegionTypes_heatmap <- NULL
        output$plot_topicsDotplot <- renderPlotly(ggplotly(p))#, tooltip="text"
      }
        output$topicCellTypes_heatmap <- renderPlot(draw(topicCellTypesHeatmap, heatmap_legend_side = "bottom"))
        
    }
  })
}

### Build page2:
page_topics <- topics.ui("plots_topics")

