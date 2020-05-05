dataPath <- "../data/"
featherFilePath <- "../dm6-regions-11species.mc8nr.feather"

### TODO: When/where to load data?
# The shinyApp function is run once, when you launch your app
#      -> If not too big, just load all objects when launching the app
# The server function is run once each time a user visits your app
#      -> Is it possible to load only the objects when needed? (e.g. when a user uses a tab/table)
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
### 

source("module_queryByRegion.server.R")

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
  
  callModule(dotPlot.server, "plot_dotplot", # same argument as to the .ui
               filePath=paste0(dataPath,"/dotPlot_highConfAnnot_UPdars.p.RData"))
  
  callModule(dotPlot.server, "plot_dotplot2", # same argument as to the .ui
             filePath=paste0(dataPath,"/dotPlot_bothAnnots.p.RData"))
  
  callModule(query_byRegion.server, "tbl_regionQueryOutput",
             featherFilePath=featherFilePath)
  
  callModule(plot_acc_tsne.server, "plot_cistrome_accessibility")
  
  ### Tables ----
  ### TODO: split into function
  
  
  callModule(tableLoad.server, "tbl_cellInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/cellInfo.Rds"))
  
   callModule(tableLoad.server, "tbl_darCellTypes", # same argument as to the .ui
              filePath=paste0(dataPath,"/DARs_adult.Rds"),
              columnTooltip="['ATAC cluster','Region','p-value','Average fold change','% of cells in the cluster with the region accessible','% cells from other clusters with the region accessible','Adjusted p-value','Nearest gene']")

   callModule(tableLoad.server, "tbl_rnaMarkers", # same argument as to the .ui
             filePath=paste0(dataPath,"/markersRNA.Rds"))
  
   callModule(tableLoad.server, "tbl_clusterInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/clusterInfo_0.3.4.RData"),
             fileType="rdata")

   callModule(tableLoad.server, "tbl_regionInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/regionInfo.Rds"))

   callModule(tableLoad.server, "tbl_darsMotifEnrichment", # same argument as to the .ui
             filePath=paste0(dataPath,"/darsMotifEnrichment.Rds"))

   callModule(tableLoad.server, "tbl_darsMotifEnrichmentSimpl", # same argument as to the .ui
             filePath=paste0(dataPath,"/darsMotifEnrichment_auc01_simplified.Rds"))
   
   callModule(tableLoad.server, "tbl_topicsMotifEnrichment", # same argument as to the .ui
              filePath=paste0(dataPath,"/topicsAdultMotifEnrichment.Rds"))

   callModule(tableLoad.server, "tbl_genesDetectedPerCellType", # same argument as to the .ui
             filePath=paste0(dataPath,"/genesDetectedPerc.Rds"))

   callModule(tableLoad.server, "tbl_tfsPerCellType", # same argument as to the .ui
             filePath=paste0(dataPath,"/TFs_perCellType.Rds"))

   callModule(tableLoad.server, "tbl_signifRegions", # same argument as to the .ui
             filePath=paste0(dataPath,"/signifRegions.Rds"))
   
   callModule(tableLoad.server, "tbl_region2geneLinks", # same argument as to the .ui
              filePath=paste0(dataPath,"/region2geneLinks.Rds"))
   

  # isolate(print(reactiveValuesToList(input)))
  # print(names(reactiveValuesToList(input)))
  # setBookmarkExclude(names=c("load_cellInfo", "load_clusterInfo", "load_darCellTypes", "load_darsMotifEnrichment", "load_darsMotifEnrichmentSimpl",
  #                            "load_genesDetectedPerCellType", "load_regionInfo", "load_rnaMarkers", "load_signifRegions", "load_tfsPerCellType",
  #                            "plot_tsne3d-plot3d_dataset", "plot_tsne3d-plot3d_perCells", "plot_tsne3d-plot3d_pntCol", "plot_tsne3d-plot3d_pntSize",
  #                            "tab_networkExample-plot_nwExample_initialized", "tab_networkExample-plot_nwExample_selected", 
  #                            "tab_plotOne-slider", "tab_plotOne-slider", 
  #                            "tbl_darCellTypes-tbl_cell_clicked", "tbl_darCellTypes-tbl_rows_all", "tbl_darCellTypes-tbl_rows_current", "tbl_darCellTypes-tbl_rows_selected", "tbl_darCellTypes-tbl_search", "tbl_darCellTypes-tbl_search_columns", "tbl_darCellTypes-tbl_state", 
  #                            "tbl_genesDetectedPerCellType-tbl_cell_clicked", "tbl_genesDetectedPerCellType-tbl_rows_all", "tbl_genesDetectedPerCellType-tbl_rows_current", "tbl_genesDetectedPerCellType-tbl_rows_selected", "tbl_genesDetectedPerCellType-tbl_search", "tbl_genesDetectedPerCellType-tbl_search_columns", "tbl_genesDetectedPerCellType-tbl_state",
  #                            "tbl_regionQueryOutput-bnt_submitRegions", "tbl_regionQueryOutput-txt_regions")) # TODO tables & anything that is too big...
  # 
  message("Server finished.")
}

