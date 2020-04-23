
### TODO: When/where to load data?
# The shinyApp function is run once, when you launch your app
#      -> If not too big, just load all objects when launching the app
# The server function is run once each time a user visits your app
#      -> Is it possible to load only the objects when needed? (e.g. when a user uses a tab/table)
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
### 

dataPath <- "/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200420_ShinyWebApp/data"
source("page_contentExamples_server.R")

## If a module needs to access an input that isn’t part of the module, the containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
#           callModule(myModule, "myModule1", reactive(input$checkbox1))
# input$file or output$file refers to the ns("file")
# Data is only loaded once (I guess it depends on when the module is called...): 
defaultTable.server <- function(input, output, session, # not optional
                               filePath,
                               fileType="rds",
                               columnTooltip=NULL) 
{
  message("loading ", filePath, "...")
  if(fileType=="rds") 
  {
    sst <- data.table(readRDS(filePath))
  }else {
    if(fileType=="rdata") 
    {
      objectName <- load(filePath)
      sst <- eval(as.name(objectName))
      rm(list=objectName)
    }else{
      stop("'fileType' not valid")
    }
  }
  print(dim(sst))
  
  ## For tooltip:
  headerCallback <- NULL
  if(!is.null(columnTooltip))  headerCallback <- JS(paste0(
                                              "function(thead, data, start, end, display){",
                                              "  var tooltips = ", columnTooltip,";",
                                              "  for(var i=0; i<=", ncol(sst),"; i++){",
                                              "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                              "  }",
                                              "}"
                                            ))
  output$tbl = DT::renderDataTable(sst, 
                                   filter="top", 
                                   escape=FALSE,
                                   server=TRUE,
                                   extensions=c("ColReorder", "FixedHeader", "Buttons"), # 
                                   options=list(
                                     pageLength = 25
                                     , colReorder=TRUE
                                     , dom = 'riftBpl'
                                     , buttons=c('copy', 'csv', 'pdf')
                                     , scrollX=TRUE
                                     # , scrollY=TRUE # vertical scroll bar within the table
                                     , fixedHeader = TRUE # header visible while scrolling
                                     , headerCallback = headerCallback
                                   )
  )
  
  # shinyServer(function(input, output) {
  #   
  #   iris1 <- reactive({
  #     iris %>% 
  #       filter(Species %in% input$species)
  #   })
  #   
  # output$table1 <- renderDataTable(iris1(), 
  # })
}


### Run server ----
server <- function(input, output, session) {
  # datafile <- callModule(csvFile, "datafile", stringsAsFactors = FALSE)
  # 
  # output$table <- renderDataTable({
  #   datafile()
  # })
  
  ## Examples ----
  callModule(histPlot.server, "tab_plotOne")
  callModule(sampleNetwork.server, "tab_networkExample") # same argument as to the .ui
  
  ## Plots ---- 
  callModule(plot3d.server, "plot_tsne3d", dataPath)
  
  ### Tables ----
  ### TODO: split into function
  callModule(defaultTable.server, "tbl_cellInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/cellInfo.Rds"))

  callModule(defaultTable.server, "tbl_darCellTypes", # same argument as to the .ui
             filePath=paste0(dataPath,"/DARs_adult.Rds"),
             columnTooltip="['ATAC cluster','Region','p-value','Average fold change','% of cells in the cluster with the region accessible','% cells from other clusters with the region accessible','Adjusted p-value','Nearest gene']")

  callModule(defaultTable.server, "tbl_rnaMarkers", # same argument as to the .ui
             filePath=paste0(dataPath,"/markersRNA.Rds"))

  callModule(defaultTable.server, "tbl_clusterInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/clusterInfo_0.3.4.RData"),
             fileType="rdata")


  callModule(defaultTable.server, "tbl_regionInfo", # same argument as to the .ui
             filePath=paste0(dataPath,"/regionInfo.Rds"))

  callModule(defaultTable.server, "tbl_darsMotifEnrichment", # same argument as to the .ui
             filePath=paste0(dataPath,"/darsMotifEnrichment.Rds"))

  callModule(defaultTable.server, "tbl_darsMotifEnrichmentSimpl", # same argument as to the .ui
             filePath=paste0(dataPath,"/darsMotifEnrichment_auc01_simplified.Rds"))


  callModule(defaultTable.server, "tbl_genesDetectedPerCellType", # same argument as to the .ui
             filePath=paste0(dataPath,"/genesDetectedPerc.Rds"))

  callModule(defaultTable.server, "tbl_tfsPerCellType", # same argument as to the .ui
             filePath=paste0(dataPath,"/TFs_perCellType.Rds"))

  callModule(defaultTable.server, "tbl_signifRegions", # same argument as to the .ui
             filePath=paste0(dataPath,"/signifRegions.Rds"))

  message("Finished loading data.")
}

