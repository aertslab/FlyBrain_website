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
page_nw_tblsRNA <- nwMotifEnr.ui("nw_RNAmarkers") 

### Server ---- 
nwRNA.server <- function(input, output, session, dataPath, tablesAlreadyLoaded) {
  observe({
    # Get settings
    tableSelection <- input$tableSelection 
    print(tableSelection)
    print(names(output))
    
    # Load table if needed:
    if(tableSelection=="Markers"){
      print(tableSelection)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_RNAmarkers.Rds"), fileType="rds")
      columnTooltip=NULL
      columnFilters=NULL
    }
    if(tableSelection=="Detected"){
      print(tableSelection)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_RNAgenesDetected.Rds"), fileType="rds") # TODO update
      dtContent <- data.table(matrix())
      columnTooltip=NULL
      columnFilters=NULL
    }
    
    # Show table (but dont trigger this event again): 
    isolate({
    output$tbl_RNA <- tableRender(dtContent, columnTooltip=columnTooltip, columnFilters=columnFilters)
    })
  })
}

