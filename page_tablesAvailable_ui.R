library(data.table)
### Prepare components ----
defaultTable.ui <- function(id){ 
  ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
  DT::dataTableOutput(ns("tbl")) #%>% withSpinner(color="#0dc5c1")
}

query_byRegion.ui <- function(id){
  ns <- NS(id)
  fluidPage(
    textAreaInput(ns("txt_regions"), "Regions", "chrN:start-end", width = "1000px"),
    actionButton(ns("bnt_submitRegions"), "Search")
  )
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
                       actionButton("load_darCellTypes", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_darCellTypes")
              ),
              
              # Tab:
              tabPanel("RNA markers",
                       id = "TablesAvailable-tabRNAmarkers",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_RNAmarkers.Rmd"),
                       actionButton("load_rnaMarkers", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_rnaMarkers")
              ),
              
              # Tab:
              tabPanel("Cluster info",
                       id = "TablesAvailable-tabClInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_clusterInfo.Rmd"),
                       actionButton("load_clusterInfo", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_clusterInfo")
              ),
              
              # Tab:
              tabPanel("Cell info",
                       id = "TablesAvailable-tabCellInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_cellInfo.Rmd"),
                       actionButton("load_cellInfo", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_cellInfo")
              ),
              
              # Tab:
              tabPanel("Region info",
                       id = "TablesAvailable-tabRegionInfo",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_regionInfo.Rmd"),
                       actionButton("load_regionInfo", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_regionInfo")
              ),
              
              # Tab:
              tabPanel("Motif enrichment",
                       id = "TablesAvailable-tabDarsMotifEnrichment",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichment.Rmd"),
                       actionButton("load_darsMotifEnrichment", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_darsMotifEnrichment")
              ),
              
              # Tab:
              tabPanel("Motif enrichment (simplified)",
                       id = "TablesAvailable-tabDarsMotifEnrichmentSimpl",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichmentSimpl.Rmd"),
                       actionButton("load_darsMotifEnrichmentSimpl", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_darsMotifEnrichmentSimpl")
              ),
              
              # Tab:
              tabPanel("Genes detected by cell type",
                       id = "TablesAvailable-tabGgenesDetectedPerCellType",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_genesDetectedPerCellType.Rmd"),
                       actionButton("load_genesDetectedPerCellType", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_genesDetectedPerCellType")
              ),
              
              # Tab:
              tabPanel("TFs per cell type",
                       id = "TablesAvailable-tabTfsPerCellType",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_tfsPerCellType.Rmd"),
                       actionButton("load_tfsPerCellType", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_tfsPerCellType")
              ),
              
              # Tab:
              tabPanel("Regions per motif",
                       id = "TablesAvailable-tabSignifRegions",
                       includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                       actionButton("load_signifRegions", "Show table"),  # TODO remove once it is loaded 
                       defaultTable.ui("tbl_signifRegions")
              ),
              
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
