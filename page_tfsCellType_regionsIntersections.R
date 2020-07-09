library(shiny)
library(plotly)
# library(dplyr)

### Prepare components ----
regionsIntersections.ui <- function(id){
  fluidPage(
    fluidRow(
      column(3,selectInput(inputId=NS(id, "sel_cistromeType"),
                         label = "Cistrome type:",
                         choices = NULL,
                         selectize = TRUE)),
      column(3,selectInput(inputId=NS(id, "sel_cellTypeTF"),
                         label = "Cell type or TF:",
                         choices = NULL,
                         selectize = TRUE)),
      column(3,checkboxInput(inputId=NS(id, "transposeHeatmap"), label="Transpose", value=FALSE))
      ),
    fluidRow(
      plotlyOutput(NS(id, "heatmap_regionOverlap"), height="700px") %>% withSpinner(color="#0dc5c1")
    )
  )
}

### Cistrome intersections
regionsIntersections.server <- function(input, output, session, dataPath) {
  #### Load files
  source('libs/cistromes_plotRegionHeatmap.R')
  load("../data/cistromes_ctOrder")
  load("../data/colVars_0.4.1.RData")
  cellTypes <- names(colVars$CellType_lvl1)
  
  cistromes <- list()
  cistromes[["DARs"]] <- arrow::read_feather(file="../data/cistromes_Dars.feather", mmap = TRUE)
  cistromes[["topics"]] <- arrow::read_feather(file="../data/cistromes_Topics.feather", mmap = TRUE)
  cistromes[["peaks"]] <- arrow::read_feather(file="../data/cistromes_Peaks.feather", mmap = TRUE)
  updateSelectInput(session, "sel_cistromeType", choices=names(cistromes), selected="DARs")
  
  plotOptions <- c(intersect(cellTypes, rbindlist(cistromes)$cellType), paste0(unique(rbindlist(cistromes)$TF), " (TF)"))
  updateSelectInput(session, "sel_cellTypeTF", choices=plotOptions, selected="T4")
  
  ### Plot
  observe({
    # Get settings
    cistromeType <- input$sel_cistromeType 
    filtValue <- input$sel_cellTypeTF
    filtType <- "cellType"
    if(grepl("(TF)", filtValue, fixed=T))
    {
      filtType <- "TF"
      filtValue <- gsub(" (TF)", "", filtValue, fixed=T)
    }
    transposeHeatmap <- !input$transposeHeatmap

    if((filtValue!="") && (filtValue %in% unlist(cistromes[[cistromeType]][,filtType])))
    {
      p <- plotRegionHeatmap(cistromes[[cistromeType]], filtValue=filtValue, filtType=filtType, ctOrder=ctOrder[[cistromeType]], transpose=transposeHeatmap)
      output$heatmap_regionOverlap <- renderPlotly(p)
    }else{
      output$heatmap_regionOverlap <- NULL
    }
  })
}

### Build page2: 
page_tfsCellType_regionsIntersections <- regionsIntersections.ui("plot_regionsHeatmap") 

