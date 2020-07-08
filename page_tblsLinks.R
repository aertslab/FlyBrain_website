library(shiny)
library(plotly)
source("page_tablesAvailable.R")

### Prepare components ----
tblsLinks.ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(3, selectInput(inputId=NS(id, "tableSelection"), label = "", choices=c("Predefined regulatory regions (CTX)","Peaks in brain"), selected = "Predefined regulatory regions (CTX)", selectize = TRUE),
      )),
    fluidRow(
      DT::dataTableOutput(ns("tbl_links")) %>% withSpinner(color="#0dc5c1")
    )
  )
}

### Build page ----
page_tblsLinks <- tblsLinks.ui("tab_Region2geneLinks") 

### Server ---- 
tblsLinks.server <- function(input, output, session, dataPath, tablesAlreadyLoaded) {
  observe({
    # Get settings
    tableSelection <- input$tableSelection 
   
    if(!is.null(tableSelection)) {
      # Load table if needed:
      if(tableSelection=="Predefined regulatory regions (CTX)"){
        dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_region2geneLinks_ctx.feather"), fileType="feather")
        columnTooltip=NULL
        columnFilters=NULL
      }
      if(tableSelection=="Peaks in brain"){
        dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_region2geneLinks_peaks.feather"), fileType="feather")
        columnTooltip=NULL
        columnFilters=NULL
      }
      
      # Show table (but dont trigger this event again): 
      isolate({
      output$tbl_links <- tableRender(dtContent, columnTooltip=columnTooltip, columnFilters=columnFilters)
      })
    }
  })
}

