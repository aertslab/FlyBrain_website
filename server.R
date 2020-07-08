dataPath <- "../data/"
featherFilePath <- "../dm6-regions-11species.mc8nr.feather"

### TODO: When/where to load data?
# The shinyApp function is run once, when you launch your app
#      -> If not too big, just load all objects when launching the app
# The server function is run once each time a user visits your app
#      -> Is it possible to load only the objects when needed? (e.g. when a user uses a tab/table)
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
### 

source("aux_cleanupUrl.R")

## If a module needs to access an input that isn’t part of the module, the containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
#           callModule(myModule, "myModule1", reactive(input$checkbox1))
# input$file or output$file refers to the ns("file")
# Data is only loaded once (I guess it depends on when the module is called...): 

### Run server ----
server <- function(input, output, session) {
  ## Examples ----
  callModule(histPlot.server, "tab_plotOne")
  callModule(sampleNetwork.server, "tab_networkExample") # same argument as to the .ui
  
  ## Plots ---- 
  callModule(plot3d.server, "plot_tsne3d", dataPath)
  
  callModule(dotPlot.server, "plot_dotplots", dataPath)  # same argument as to the .ui
  
  callModule(query_byRegion.server, "tbl_regionQueryOutput", featherFilePath=featherFilePath)
  
  callModule(plot_tf_details.server, "tfDetails", dataPath)
  callModule(dotPlot.server, "plot_dotplots", dataPath)  # same argument as to the .ui
  callModule(regionsIntersections.server, "plot_regionsHeatmap", dataPath)
  
  ### Load when tab is clicked ----
  tablesAlreadyLoaded <- reactiveVal("")
  observe({ 
    # isolate(print(reactiveValuesToList(input)))
    # inputNames <- names(reactiveValuesToList(input))
    # print(inputNames)
    
    #### TablesAvailable  ----
    if(input[["downloads-tab"]] == "CellInfo"){
      tblNames <- callModule(tableLoad.server, "tbl_CellInfo", # same argument as to the .ui
                             filePath=paste0(dataPath,"/cellInfo.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    } 
    if(input[["CellTypes_TFs-tab"]] == "ClInfo") {
      tblNames <- callModule(tableLoad.server, "tbl_ClInfo", # same argument as to the .ui
                              filePath=paste0(dataPath,"/clusterInfo_0.3.4.RData"),
                             fileType="rdata", tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    } 
    if(input[["networks_tables"]] == "nw_RNAmarkers"){
        tblNames <- callModule(tableLoad.server, "tbl_RNA", # same argument as to the .ui
                filePath=paste0(dataPath,"/tbl_RNAmarkers.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
        tablesAlreadyLoaded(tblNames)
        # To switch table with the selector:
        callModule(nwRNA.server, "nw_RNAmarkers", dataPath, tablesAlreadyLoaded=tablesAlreadyLoaded())
    } else if(input[["networks_tables"]] == "DARs"){
      tblNames <- callModule(tableLoad.server, "tbl_DARs", # same argument as to the .ui
              filePath=paste0(dataPath,"/tbl_DARs.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
              # columnTooltip="['ATAC cluster','Region','p-value','Average fold change','% of cells in the cluster with the region accessible','% cells from other clusters with the region accessible','Adjusted p-value','Nearest gene']")
      tablesAlreadyLoaded(tblNames)
    } else if(input[["networks_tables"]] == "nw_motifEnrichment"){
      tblNames <- callModule(tableLoad.server, "tbl_MotifEnrichment", # same argument as to the .ui
              filePath=paste0(dataPath,"/tbl_DARs_motifEnr_auc01_simplified.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
      # To switch table with the selector:
      callModule(nwMotifEnr.server, "nw_motifEnrichment", dataPath,tablesAlreadyLoaded=tablesAlreadyLoaded())
    } else if(input[["networks_tables"]] == "TfsPerCellType"){
      tblNames <- callModule(tableLoad.server, "tbl_TfsPerCellType", # same argument as to the .ui
              filePath=paste0(dataPath,"/TFs_perCellType.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    } else if(input[["networks_tables"]] == "SignifRegions"){
      tblNames <- callModule(tableLoad.server, "tbl_SignifRegions", # same argument as to the .ui
              filePath=paste0(dataPath,"/signifRegions.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    } else if(input[["networks_tables"]] == "RegionInfo"){
      tblNames <- callModule(tableLoad.server, "tbl_RegionInfo", # same argument as to the .ui
              filePath=paste0(dataPath,"/regionInfo.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    } else if(input[["networks_tables"]] == "Region2geneLinks"){
      tblNames <- callModule(tableLoad.server, "tbl_Region2geneLinks", # same argument as to the .ui
              filePath=paste0(dataPath,"/region2geneLinks.Rds"), tablesAlreadyLoaded=tablesAlreadyLoaded())
      tablesAlreadyLoaded(tblNames)
    }
  })
   
  ### Bookmark management ----
  donotBookmark <- c(
   # Region to query:
   "tbl_regionQueryOutput-txt_regions", "tbl_regionQueryOutput-bnt_submitRegions",
   
   # To decide whether to bookmark or not:
   "plot_tsne3d-plot3d_perCells","plot_tsne3d-plot3d_dataset", "plot_tsne3d-plot3d_pntCol", "plot_tsne3d-plot3d_pntSize",
   ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", # in TFs tab
   
   ## Menu state (to decide)
   "sidebarCollapsed", "sidebarItemExpanded"
  )
  setBookmarkExclude(names=donotBookmark) # TODO tables & anything that is too big...
  
  # Update URL
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    url <- cleanupUrl(url)
    updateQueryString(url)
  })
  
  ### End ---- 
  message("Server finished.")
}

