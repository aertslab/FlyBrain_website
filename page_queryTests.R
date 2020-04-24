library(data.table)
### Prepare components ----
query_byRegion.ui <- function(id){
  ns <- NS(id)
  fluidPage(
    textAreaInput(ns("txt_regions"), "Regions", placeholder="chr2L:4867305-4868251 (one per line)"), #, height = "300px"
    actionButton(ns("bnt_submitRegions"), "Search")
  )
}

### Build page ----
page_queryTests <- fluidPage(
  tabsetPanel(type = "tabs",
              id = "Queries-tabset",
              # Tab:
              tabPanel("Query test",
                       id = "TablesAvailable-tabQueryByRegion",
                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                       query_byRegion.ui("tbl_regionQueryOutput")
              )
              
              ### Other data available
              # Janelia images / Braincode?
              # AUCell viewer?
              # Topics viewer?
              # the TF-cellType heatmap
  )
)
