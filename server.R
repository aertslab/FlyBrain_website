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
  
  callModule(plot_tf_details.server, "plotTF")
  
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
   
   
   
  ### Bookmark management ---- 
  # isolate(print(reactiveValuesToList(input)))
  # inputNames <- names(reactiveValuesToList(input))
  ###
  tablesWithLoadButton <- c("tbl_regionInfo", "tbl_topicsMotifEnrichment", "tbl_clusterInfo", "tbl_darCellTypes", "tbl_darsMotifEnrichment",
                           "tbl_tfsPerCellType", "tbl_darsMotifEnrichmentSimpl", "tbl_cellInfo", "tbl_rnaMarkers", "tbl_region2geneLinks",
                           "tbl_genesDetectedPerCellType","tbl_signifRegions")
  donotBookmark <- c(
   # Region to query:
   "tbl_regionQueryOutput-txt_regions", "tbl_regionQueryOutput-bnt_submitRegions",
   
   # To decide whether to bookmark or not:
   "plot_tsne3d-plot3d_perCells","plot_tsne3d-plot3d_dataset", "plot_tsne3d-plot3d_pntCol", "plot_tsne3d-plot3d_pntSize",
   ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", # in TFs tab
   
   ## Menu state (to decide)
   "sidebarCollapsed", "sidebarItemExpanded",
   
   # From tables:
   paste0(tablesWithLoadButton, "-load"), # load button
   paste0(tablesWithLoadButton, "-tbl_rows_all"), # Needs to be ignored, it is huge!
   paste0(tablesWithLoadButton, "-tbl_search"),
   paste0(tablesWithLoadButton, "-tbl_search_columns"),
   paste0(tablesWithLoadButton, "-tbl_state"),
   paste0(tablesWithLoadButton, "-tbl_cell_clicked"),
   paste0(tablesWithLoadButton, "-tbl_rows_selected"),
   paste0(tablesWithLoadButton, "-tbl_row_last_clicked"),
   paste0(tablesWithLoadButton, "-tbl_rows_current") ## maybe can be kept?
  )
  setBookmarkExclude(names=donotBookmark) # TODO tables & anything that is too big...
  
  # Update URL
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    # Keep only the tab for the current page: 
    # url="http://localhost:24514/p/e373a11f/?_inputs_&page=%22Networks%22&Figures-tab=%223D%22&Tfs-tab=%22dotplot1%22&query-tab=%22Query%22&plots_acc_barplots_nes_expr-tf=%22ey%22&Table=%22CellInfo%22&cellTypes-tab=%22descr%22"
    url <- strsplit(url,"&")[[1]]
    baseUrl <- c(url[1], grep("page=", url, value=T))
    tabsSelected <- grep("-tab", url, value=T)
    # remainingSettings <- url[which(!url %in% c(baseUrl,tabsSelected))] # at some point keep only settings for the current tab...
    remainingSettings <- NULL # Other settings dont work (probably because they are included in other functions...)
    
    currentTab <- gsub("%22", "", gsub("page=", "", grep("page=", baseUrl, value=T)))
    currentTab <- grep(currentTab, tabsSelected,value=T)
    
    url <- paste0(c(baseUrl, remainingSettings), collapse="&")
    if(length(currentTab) > 0) url <- paste0(c(baseUrl, currentTab, remainingSettings), collapse="&")
    ##
    
    print(url)
    updateQueryString(url)
  })
  
  ### End ---- 
  message("Server finished.")
}

