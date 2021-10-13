library(shiny)
library(plotly)
source("page_tablesAvailable.R")

### Prepare components ----
nwRNA.ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(2, selectInput(inputId=NS(id, "tableSelection"), label = "", choices=c("Markers","Detected"), selected = "Markers", selectize = TRUE),
      )),
    fluidRow(
      DT::dataTableOutput(ns("tbl_RNA")) %>% withSpinner(color="#0dc5c1")
    )
  )
}

### Build page ----
page_nw_tblsRNA <- nwRNA.ui("tab_RNAmarkers") 

### Server ---- 
nwRNA.server <- function(input, output, session, dataPath, tablesAlreadyLoaded) {
  observe({
    # Get settings
    tableSelection <- input$tableSelection 
   
    if(!is.null(tableSelection)) {
      # Load table if needed:
      if(tableSelection=="Markers"){
        dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_RNAmarkers.feather"), fileType="feather")
        columnTooltip=NULL
        columnFilters=NULL
      }
      if(tableSelection=="Detected"){
        dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_RNAgenesDetected.feather"), fileType="feather")
        columnTooltip=NULL
        columnFilters=NULL
      }
      
      # Show table (but dont trigger this event again): 
      isolate({
      output$tbl_RNA <- tableRender(dtContent, columnTooltip=columnTooltip, columnFilters=columnFilters)
      })
    }
  })
}

