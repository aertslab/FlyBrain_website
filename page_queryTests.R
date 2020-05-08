library(data.table)
### Prepare components ----
query_byRegion.ui <- function(id){
  ns <- NS(id)
  fluidPage(
    textAreaInput(ns("txt_regions"), "Regions", placeholder="chr2L:4867305-4868251 (one per line)"), #, height = "300px"
    actionButton(ns("bnt_submitRegions"), "Search")
  )
}

library(visNetwork)
sampleNetwork.ui <- function(id){
  fluidPage(
    sidebarPanel(
      "some controls here..."
    ),
    mainPanel(
      fluidRow(
        visNetworkOutput(NS(id, "plot_nwExample"), height="900px") %>% withSpinner(color="#0dc5c1"),
      )
    )
  )
}

### Other data available
# Janelia images / Braincode?
# AUCell viewer?
# Topics viewer?
# the TF-cellType heatmap
