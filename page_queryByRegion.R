library(data.table)
### UI ----
query_byRegion.ui <- function(id){
  ns <- NS(id)
  fluidPage(
    textAreaInput(ns("txt_regions"), "Regions", placeholder="chr2L:4867305-4868251 (one per line)"), #, height = "300px"
    actionButton(ns("bnt_submitRegions"), "Search"),
    textOutput(ns("status")),
    DT::dataTableOutput(ns("tbl")) 
  )
}

### Server ----
query_byRegion.server <- function(input, output, session, # not optional
                                  featherFilePath,
                                  ...) 
{
  ### Load required data ----
  # message("Loading ",session$ns(NULL))
  ## TODO simplify/prebuild or copy to data folder
  source('libs/convertToDbRegions.R')
  source('libs/keepUniquePairs.R')
  load("../data/darRegions.RData") # might be differential, but not enriched...
  enrichmentTable <- readRDS("../data/darsMotifEnrichment.Rds")
  load("../data//byCellType_regionsPerTF.RData") # ~regulon
  load("../data/byCellType_signifRegions_genesNearby.RData")
  load("../data/genesDetected_in10perc_CellTypeGroups.RData")
  markersRNA <- readRDS("../data/markersRNA.Rds")
  load("../data/atacClsList.RData")
  load("../data/regionToMotif_noBg.RData")
  load("../data/regionToMotif_withBg.RData")
  
  ### If a query is submitted ----
  observeEvent(input$bnt_submitRegions, {
      # Process input
      queryRegions <- input$txt_regions
      queryRegions <- strsplit(queryRegions, "\n|;| ")[[1]]
      queryRegions <- trimws(queryRegions)

      if(length(queryRegions) < 1)
      {
        output$status <- renderText("Empty input")
      }else{
        # bedr::determine.input()  ## TODO
        output$status <- renderText("Loading...")

        # Retrieve info and make table:
        infoTable <- getRegionInfo(queryRegions, featherFilePath,
                                   darRegions, regionsPerTF, signifRegions_genesNearby, genesDetectedList, markersRNA, atacClsList,
                                   regionToMotif_noBg, regionToMotif_withBg, enrichmentTable)
        output$tbl <- DT::renderDataTable(infoTable,
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
                                           # , headerCallback = headerCallback
                                         )
        )
        output$status <- renderText("")
      }
  })
}


### Aux functions ----
# queryRegions <- c("chr2L:4866131-4867303", "chr3R:26711114-26711413")
getRegionInfo <- function(queryRegions, featherFilePath,
                          darRegions, regionsPerTF, signifRegions_genesNearby, genesDetectedList, markersRNA, atacClsList,
                          regionToMotif_noBg, regionToMotif_withBg, enrichmentTable)
{
  print("building...")
  met_theseRegions <- list()
  infoCols <- c("queryRegion"
                # ,"size", "Positive","ExpectedCellType","Flylight","Category"
                ,"ctx"
                ,"DAR"
                ,"TFsExpressedWithMotif"
                ,"nMotifsExpTF", "maxNes", "topMotif"
                ,"markersNearby"
                ,"genesDetected10perc"
                ,"nGenesNotDetected")
  
  infoTable <- as.data.frame(matrix(NA,nrow=0, ncol=length(infoCols), dimnames = list(NULL, infoCols)))
  for(queryRegion in queryRegions)
  {
    regions <- convertToDbRegions(list(queryRegion), featherFilePath=featherFilePath, verbose=F)
    regions <- unique(gsub("dmel_r6.02__", "", regions[[1]]))
    
    for(region in regions){
      darSetsName <- names(darRegions)[which(sapply(darRegions, function(x) any(region %in%x)))] # DAR
      
      if(length(darSetsName)==0)
      {
        infoTable[nrow(infoTable)+1,] <- rep("", ncol(infoTable))
        infoTable[nrow(infoTable),"queryRegion"] <- queryRegion
        infoTable[nrow(infoTable),"ctx"] <- region
        infoTable[nrow(infoTable),"DAR"] <- "NONE"
        # could add genes nearby...
      }
      
      for(setName in darSetsName)
      {
        TFs <- names(regionsPerTF[[setName]])[which(sapply(regionsPerTF[[setName]], function(x) any(region %in% unlist(x))))]
        
        ### Genes nearby that are markers?
        genesNearby <- signifRegions_genesNearby[[setName]][[region]]
        
        cellType <- strsplit(setName, "__")[[1]][[1]]
        # Detected:
        genesDetected <- sort(unique(unname(unlist(genesDetectedList[[cellType]]))))
        genesDetected <- intersect(genesNearby, genesDetected)
        
        # markers:
        markers_SS <- markersRNA[which(markersRNA$gene %in% genesNearby),]
        rnaCls <- atacClsList[[cellType]] # They were already renamed to ATAC's
        markers_SS <- markers_SS[which(markers_SS$cluster %in% rnaCls),]
        genesDetected <- genesDetected[which(!genesDetected %in% markers_SS$gene)]
        genesNearby <- genesNearby[which(!genesNearby%in% genesDetected)]
        
        # Save: 
        infoTable[nrow(infoTable)+1,] <- rep("", ncol(infoTable))
        infoTable[nrow(infoTable),"queryRegion"] <- queryRegion
        infoTable[nrow(infoTable),"ctx"] <- region
        infoTable[nrow(infoTable),"DAR"] <- setName
        infoTable[nrow(infoTable),"TFsExpressedWithMotif"] <- paste0(TFs, collapse=";  ")
        infoTable[nrow(infoTable),"markersNearby"] <- paste(markers_SS$gene, collapse="; ")
        infoTable[nrow(infoTable),"genesDetected10perc"] <- paste(genesDetected, collapse="; ")
        infoTable[nrow(infoTable),"nGenesNotDetected"] <- length(genesNearby)
        
        ### Motifs...
        motifsEnriched <- regionToMotif_noBg[[setName]][[region]]
        motifsEnriched <- sort(unique(c(motifsEnriched, regionToMotif_withBg[[setName]][[region]])))
        
        metSS <- enrichmentTable[which((enrichmentTable$motif %in% motifsEnriched) & (enrichmentTable$darSet %in% setName)),]; nrow(metSS)
        
        if(nrow(metSS)>0)
        {
          metSS <- keepUniquePairs(metSS, col1="cellType", verbose=FALSE)
          metSS<- metSS[order(metSS$NES),]
          # met_theseRegions[[region]] <- rbind(met_theseRegions[[region]], metSS)
          
          infoTable[nrow(infoTable),"nMotifsExpTF"] <- nrow(metSS)
          infoTable[nrow(infoTable),"maxNes"] <- metSS$NES[1]
          infoTable[nrow(infoTable),"topMotif"] <- as.character(metSS$motif[1])
        }
      }
    }
  }
  infoTable <- infoTable[order(infoTable$maxNes, decreasing=T),]
  infoTable$queryRegion <- factor(infoTable$queryRegion)
  infoTable$nMotifsExpTF <- as.numeric(infoTable$nMotifsExpTF)
  infoTable <- RcisTarget::addLogo(infoTable, motifCol="topMotif", dbVersion="v9dl")
  infoTable$topMotif <- infoTable$logo
  infoTable <- infoTable[,-1]
  return(infoTable)
}
