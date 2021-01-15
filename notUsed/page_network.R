library(data.table)
library(visNetwork)
sampleNetwork.ui <- function(id){
  fluidPage(
    mainPanel(
      fluidRow(
        visNetworkOutput(NS(id, "plot_nwExample"), height="900px") %>% withSpinner(color="#0dc5c1"),
      )
    )
    # sidebarPanel(
    #   "some controls here..."
    # )
  )
}

### Other data available
# Janelia images / Braincode?
# AUCell viewer?
# Topics viewer?
# the TF-cellType heatmap
