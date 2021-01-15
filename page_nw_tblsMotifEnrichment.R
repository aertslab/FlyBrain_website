library(shiny)
library(plotly)
source("page_tablesAvailable.R")

### Prepare components ----
nwMotifEnr.ui <- function(id){
  ns <- NS(id)
  
  settingsNameChoices <- c(
    "DARs (simplified)",
    "DARs (all motifs)",
    "Peaks",
    "Topics"
  ) 
  
  fluidPage(
    fluidRow(
      column(2, selectInput(inputId=NS(id, "met2show"), label = "", choices=settingsNameChoices, selected="DARs (simplified)", selectize=TRUE),
      )),
    fluidRow(
      # tableLoad.ui("tbl_MotifEnrichment") # does not reload...
      DT::dataTableOutput(ns("tbl_MotifEnrichment")) %>% withSpinner(color="#0dc5c1")
    )
  )
}

### Build page ----
page_nw_tblsMotifEnrichment <- nwMotifEnr.ui("tab_motifEnrichment") 

### Server ---- 
nwMotifEnr.server <- function(input, output, session, dataPath, tablesAlreadyLoaded) {
  observe({
    # Get settings
    met2show <- input$met2show 
    print(met2show)
    print(names(output))
    
    # Load table if needed:
    if(met2show=="DARs (simplified)"){
      print(met2show)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_motifEnr_DARs_auc01_simplified.Rds"), fileType="rds")
      columnTooltip=NULL
      columnFilters=NULL
    }
    if(met2show=="DARs (all motifs)"){
      print(met2show)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_motifEnr_DARs.Rds"), fileType="rds")
      columnTooltip=NULL
      columnFilters=list(me_rocThr=c('["auc01","auc05"]')) # NES='5 ... 50'
    }
    if(met2show=="Peaks"){
      print(met2show)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_motifEnr_peaks.Rds"), fileType="rds")
      columnTooltip=NULL
      columnFilters=NULL
    }
    if(met2show=="Topics"){
      print(met2show)
      dtContent <- tableLoad(filePath=paste0(dataPath,"/tbl_motifEnr_topics.Rds"), fileType="rds")
      columnTooltip=NULL
      columnFilters=NULL
    }
    
    
    # Show table (but dont trigger this event again): 
    isolate({
      output$tbl_MotifEnrichment <- tableRender(dtContent, columnTooltip=columnTooltip, columnFilters=columnFilters)
    })
  })
}

