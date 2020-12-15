getRegionInfo <- function(queryRegions, 
                          signifRegions,
                          region2geneLinks,
                          genesDetectedPerc, 
                          cistromes, 
                          enrichmentTable, 
                          dars
)
  # featherFilePath,
  # dars, regionsPerTF, signifRegions_genesNearby, genesDetectedList, markersRNA, atacClsList,
  # regionToMotif_noBg, regionToMotif_withBg, enrichmentTable)
{
  print("building...")
  allRegions <- GRanges(unique(dars$region))
  met_theseRegions <- list()
  infoCols <- c("queryRegion"
                # ,"size", "Positive","ExpectedCellType","Flylight","Category"
                ,"ctx/peak"
                ,"DAR"
                ,"genesNearbyExpressed"
                ,"nGenesNotDetected"
                ,"inCistrome"
                ,"nMotifsExpTF", "maxNes", "topMotif"
                # ,"markersNearby"
  )
  
  infoTable <- as.data.frame(matrix(NA,nrow=0, ncol=length(infoCols), dimnames = list(NULL, infoCols)))
  nRegions <- length(queryRegions)
  # withProgress(message = paste("Loading region ", 1, "/", nRegions, "..."), value = 0, {
  for(i in seq_along(queryRegions))
  {
    print(i)
    queryRegion <- queryRegions[i]
    regions <- unique(convertToTargetRegions(queryRegion, allRegions, verbose=F))
    
    for(regionLoc in regions){
      message(regionLoc)
      darCellTypes <- dars %>% dplyr::filter(region %in% regionLoc) %>% dplyr::select(cellType) %>% distinct()
      darCellTypes <- as.character(unlist(darCellTypes))
      
      if(length(darCellTypes)==0)
      {
        infoTable[nrow(infoTable)+1,] <- rep("", ncol(infoTable))
        infoTable[nrow(infoTable),"queryRegion"] <- queryRegion
        infoTable[nrow(infoTable),"ctx"] <- region
        infoTable[nrow(infoTable),"DAR"] <- "NONE"
        # could add genes nearby...
      }else{
        for(darCellType in darCellTypes)
        {
          TFs <- cistromes %>% 
            dplyr::filter(cellType %in% darCellType) %>%
            dplyr::filter(region %in% regionLoc) %>%
            dplyr::select(TF) %>% distinct()
          TFs <- unname(unlist(TFs))
          
          ### Genes nearby that are markers/expressed?
          genesNearby <- unname(unlist(
            region2geneLinks %>% 
              dplyr::filter(regRegion %in% regionLoc) %>%
              dplyr::select(targetGene) %>% distinct()))
          
          # Detected:
          genesDetected <- sort(unique(unname(unlist(genesDetectedPerc[[darCellType]]))))
          genesDetected <- intersect(genesNearby, genesDetected)
          
          # # markers: # TODO
          # markers_SS <- markersRNA[which(markersRNA$gene %in% genesNearby),]
          # rnaCls <- atacClsList[[cellType]] # They were already renamed to ATAC's
          # markers_SS <- markers_SS[which(markers_SS$cluster %in% rnaCls),]
          # genesDetected <- genesDetected[which(!genesDetected %in% markers_SS$gene)]
          genesNearby <- genesNearby[which(!genesNearby%in% genesDetected)]
          
          # Save: 
          infoTable[nrow(infoTable)+1,] <- rep("", ncol(infoTable))
          infoTable[nrow(infoTable),"queryRegion"] <- queryRegion
          infoTable[nrow(infoTable),"ctx/peak"] <- regionLoc
          infoTable[nrow(infoTable),"DAR"] <- darCellType
          infoTable[nrow(infoTable),"inCistrome"] <- paste0(TFs, collapse=";  ")
          # infoTable[nrow(infoTable),"markersNearby"] <- paste(markers_SS$gene, collapse="; ")
          infoTable[nrow(infoTable),"genesNearbyExpressed"] <- paste(genesDetected, collapse="; ")
          infoTable[nrow(infoTable),"nGenesNotDetected"] <- length(genesNearby)
          
          ### Motifs...
          motifsEnriched <- as.character(unlist(
            signifRegions %>%
              dplyr::filter(cellType %in% darCellType) %>%
              dplyr::filter(region %in% regionLoc) %>%
              select("idEnriched") %>% distinct()
          ))
          
          # Only motifs for TFs with cistromes:
          motifsEnriched <- as.character(unlist(
            tfAnnot %>%
              dplyr::filter(motif %in% motifsEnriched) %>%
              dplyr::filter(TF %in% TFs) %>%
              select("motif") %>% distinct()
          ))
          
          metSS <- enrichmentTable %>%
            dplyr::filter(cellType %in% darCellType) %>%
            dplyr::filter(motif %in% motifsEnriched) %>%
            select("motif", "NES") %>% 
            distinct()
          
          if(nrow(metSS)>0)
          {
            infoTable[nrow(infoTable),"nMotifsExpTF"] <- nrow(metSS)
            infoTable[nrow(infoTable),"maxNes"] <- max(metSS$NES)
            infoTable[nrow(infoTable),"topMotif"] <- as.character(metSS$motif[which.max(metSS$NES)])
          }
        } 
      }
    }
    # incProgress(1/nRegions, message=paste("Loading region ", min(i+1, nRegions), "/", nRegions, "..."))
  }
  # })
  
  infoTable <- infoTable[order(infoTable$maxNes, decreasing=T),]
  infoTable$queryRegion <- factor(infoTable$queryRegion)
  infoTable$nMotifsExpTF <- as.numeric(infoTable$nMotifsExpTF)
  infoTable <- RcisTarget::addLogo(infoTable, motifCol="topMotif", dbVersion="v9dl")
  infoTable$topMotif <- infoTable$logo
  infoTable <- infoTable[,-1]
  return(infoTable)
}
