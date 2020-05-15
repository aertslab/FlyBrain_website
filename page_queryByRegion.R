library(data.table)
### UI ----
query_byRegion.ui <- function(id){
  ns <- NS(id)
  
  #############
  # TODO: Add options/settings box
  # Regions for genes: linked to gene expression (50kbp) or 5kbpUpstreamPlusIntrons
  # Columns to show... (to reduce running time...? )
  # Max regions Default: 10 (otherwise takes long...)
  #######
  fluidPage(
    textAreaInput(ns("txt_regions"), "Regions or gene", placeholder="chr2L:4867305-4868251 (one per line)"), #, height = "300px"
    actionButton(ns("bnt_submitRegions"), "Search"),
    span(textOutput(ns("statusBad")), style="color:red"),
    span(textOutput(ns("statusGood")), style="color:blue"),
    DT::dataTableOutput(ns("tbl"))
  )
}

### Server ----
query_byRegion.server <- function(input, output, session, # not optional
                                  featherFilePath,
                                  ...) 
{
  nMax <- 10
  ### Load required data ----
  # message("Loading ",session$ns(NULL))
  ## TODO simplify/prebuild or copy to data folder
  source('libs/convertToDbRegions.R')
  source('libs/keepUniquePairs.R')
  load("../data/darRegions.RData") # might be differential, but not enriched...
  enrichmentTable <- readRDS("../data/darsMotifEnrichment.Rds")
  enrichmentTable$darSet <- paste0(enrichmentTable$cellType,"__",enrichmentTable$DAR_type)
  load("../data/byCellType_regionsPerTF.RData") # ~regulon
  load("../data/byCellType_signifRegions_genesNearby.RData")
  load("../data/genesDetected_in10perc_CellTypeGroups.RData")
  markersRNA <- readRDS("../data/markersRNA.Rds")
  load("../data/atacClsList.RData")
  load("../data/regionToMotif_noBg.RData")
  load("../data/regionToMotif_withBg.RData")
  region2geneLinks <- readRDS("../data/region2geneLinks.Rds")
  load("../data/ctxRegionsPerGene_v1.0_5kbp.RData")
  
  ### If a query is submitted ----
  
  observeEvent(input$bnt_submitRegions, {
      # Process input
      queryRegions <- input$txt_regions
      queryRegions <- strsplit(queryRegions, "\n|;|,|\t| ")[[1]]
      queryRegions <- trimws(queryRegions)
      
      if(length(queryRegions) < 1)
      {
        output$statusBad <- renderText("Empty input")
        output$statusGood <- NULL
        output$tbl <- NULL
      }else if(length(queryRegions) > 10)
      {
        output$statusBad <- renderText(paste0("Too many genes/regions (", length(queryRegions), "). Please, select a maximum of ",nMax," query regions."))
        output$statusGood <- NULL
        output$tbl <- NULL
      }else{
        output$statusGood <- NULL
        output$tbl <- NULL
        
        maybeGenes <- any(!grepl("chr*\\:*\\-*", queryRegions))
        if(maybeGenes){

            # ### Option A: Link based on region-gene
            # geneRegions <- region2geneLinks[which(region2geneLinks$Gene %in% queryRegions),]
            # geneRegions <- geneRegions[which(geneRegions$LinkScore> 900), ] # add as parameter?
            # geneRegions <- as.character(geneRegions[,"Regulatory region"])

            ### Option B: Any region nearby
            geneRegions <- unique(unlist(ctxRegionsPerGene[queryRegions]))

            queryRegions <- unique(c(queryRegions, geneRegions))
            # Discard non-regions (e.g. genes)
            queryRegions <- grep("chr*\\:*\\-*", queryRegions, value = T)
            updateTextInput(session, "txt_regions", value=paste(queryRegions, collapse="\n"))

            output$statusGood <- renderText(paste0("Genes converted to ", length(queryRegions), " regions. Re-submit with a maximum of ",nMax," regions."))
            output$statusBad <- NULL

        }else{
          # bedr::determine.input()  ## TODO
          # output$status <- renderText("Loading...") # doesnt update it instantly...

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
        }
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
  nRegions <- length(queryRegions)
  withProgress(message = paste("Loading region ", 1, "/", nRegions, "..."), value = 0, {
    for(i in seq_along(queryRegions))
    {
      queryRegion <- queryRegions[i]
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
        }else{
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
            
            metSS <- data.frame(enrichmentTable[which((enrichmentTable$motif %in% motifsEnriched) & (enrichmentTable$darSet %in% setName)),]); nrow(metSS)
            
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
      incProgress(1/nRegions, message=paste("Loading region ", min(i+1, nRegions), "/", nRegions, "..."))
    }
  })
  
  infoTable <- infoTable[order(infoTable$maxNes, decreasing=T),]
  infoTable$queryRegion <- factor(infoTable$queryRegion)
  infoTable$nMotifsExpTF <- as.numeric(infoTable$nMotifsExpTF)
  infoTable <- RcisTarget::addLogo(infoTable, motifCol="topMotif", dbVersion="v9dl")
  infoTable$topMotif <- infoTable$logo
  infoTable <- infoTable[,-1]
  return(infoTable)
}
