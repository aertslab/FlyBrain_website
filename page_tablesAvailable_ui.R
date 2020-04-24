library(data.table)
### Prepare components ----

# # Simple table, without load button:
# defaultTable.ui <- function(id){ 
#   ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
#   DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
# }

# Load table on button press:
tableLoad.ui <- function(id){ 
  ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
  
  fluidPage(
    actionButton(ns("load"), "Show table"), 
    DT::dataTableOutput(ns("tbl"))
  )
  
  # TODO
  # Without the conditional pannel the "spinner" is already there before the button press
  # How to show it conditionally?!?!  (is it needed? - e.g. is it slow?)
  # conditionalPanel(
  #   condition = paste0("output.", ns("clicked")), # "output.tbl_darCellTypes-clicked"
  #   DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
  # )
}

### Build page ----
page_tablesAvailable <- fluidPage(
  
  h2("All tables available so far:"),
  
  tabsetPanel(type = "tabs",
              id = "TablesAvailable-tabset",
              
              # Tab: 
              tabPanel("DARs",
                       id = "TablesAvailable-tabDARs",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
                       tableLoad.ui("tbl_darCellTypes")
              ),
              
              # Tab:
              tabPanel("RNA markers",
                       id = "TablesAvailable-tabRNAmarkers",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_RNAmarkers.Rmd"),
                       tableLoad.ui("tbl_rnaMarkers")
              ),
              
              # Tab:
              tabPanel("Cluster info",
                       id = "TablesAvailable-tabClInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_clusterInfo.Rmd"),
                       tableLoad.ui("tbl_clusterInfo")
              ),
              
              # Tab:
              tabPanel("Cell info",
                       id = "TablesAvailable-tabCellInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_cellInfo.Rmd"),
                       tableLoad.ui("tbl_cellInfo")
              ),
              
              # Tab:
              tabPanel("Region info",
                       id = "TablesAvailable-tabRegionInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_regionInfo.Rmd"),
                       tableLoad.ui("tbl_regionInfo")
              ),
              
              # Tab:
              tabPanel("Motif enrichment",
                       id = "TablesAvailable-tabDarsMotifEnrichment",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichment.Rmd"),
                       tableLoad.ui("tbl_darsMotifEnrichment")
              ),
              
              # Tab:
              tabPanel("Motif enrichment (simplified)",
                       id = "TablesAvailable-tabDarsMotifEnrichmentSimpl",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichmentSimpl.Rmd"),
                       tableLoad.ui("tbl_darsMotifEnrichmentSimpl")
              ),
              
              # Tab:
              tabPanel("Genes detected by cell type",
                       id = "TablesAvailable-tabGgenesDetectedPerCellType",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_genesDetectedPerCellType.Rmd"),
                       tableLoad.ui("tbl_genesDetectedPerCellType")
              ),
              
              # Tab:
              tabPanel("TFs per cell type",
                       id = "TablesAvailable-tabTfsPerCellType",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_tfsPerCellType.Rmd"),
                       tableLoad.ui("tbl_tfsPerCellType")
              ),
              
              # Tab:
              tabPanel("Regions per motif",
                       id = "TablesAvailable-tabSignifRegions",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                       tableLoad.ui("tbl_signifRegions")
              )
              
              ### Other data available
              # Janelia images / Braincode?
              # AUCell viewer?
              # Topics viewer?
              # the TF-cellType heatmap
  )
)
