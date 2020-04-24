library(data.table)
### Prepare components ----

# Simple table, without load button:
defaultTable.ui <- function(id){ 
  ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
  DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
}


# ui <- fluidPage(
                    #   actionButton("eval","Evaluate"),
              #   numericInput("num_input", "If number is changed, cp must hide", value = 0),
#   conditionalPanel("input.eval && !output.hide_panel", "text")
# )
# 
# server <- function(input, output, session) {
#   
          #   output$hide_panel <- eventReactive(input$num_input, TRUE, ignoreInit = TRUE)
#   
#   outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
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
              
              # # Tab: 
              # tabPanel("DARs",
              #          id = "TablesAvailable-tabDARs",
              #          includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
              #          actionButton("load_darCellTypes", "Show table"),  # TODO remove once it is loaded 
              #          defaultTable.ui("tbl_darCellTypes")
              # ),
              
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
              )
              
              ### Other data available
              # Janelia images / Braincode?
              # AUCell viewer?
              # Topics viewer?
              # the TF-cellType heatmap
  )
)
