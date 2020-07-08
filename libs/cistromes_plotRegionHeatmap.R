library(ggplot2)
plotRegionHeatmap <- function(cistromes, filtValue, filtType="TF", ctOrder=NULL, transpose=TRUE)
{
  if(filtType=="cellType") varName <- "TF"
  
  if(filtType=="TF") varName <- "cellType"
  
  incidList <- cistromes %>% 
    filter(!!sym(filtType) %in% filtValue) %>% 
    dplyr::select(!!sym(varName), "region")
  incidMat <- as.data.frame.matrix(table(as.character(incidList$region), as.character(incidList[[varName]]))); dim(incidMat)
  
  clRows <- as.dendrogram(stats::hclust(proxy::dist(incidMat, method="Jaccard", by_rows=TRUE), method="ward.D2"))
  rowOrder <- labels(clRows); length(rowOrder)
  incidMat <- incidMat[rowOrder,, drop=F]
  
  if(varName=="TF"){ 
    clCols <- as.dendrogram(stats::hclust(proxy::dist(incidMat, method="Jaccard", by_rows=FALSE), method="ward.D2"))
    colOrder <- labels(clCols); length(colOrder)
    incidMat <- incidMat[,colOrder, drop=F]
  }else{
    incidMat <- incidMat[,intersect(ctOrder,unique(incidList$cellType)), drop=F]
  }
  
  if(transpose) incidMat <- t(incidMat)
  p <- plot_ly(z = data.matrix(incidMat), 
          x = colnames(incidMat), 
          y = row.names(incidMat), 
          colors = c("white", "darkblue"), showscale=F,
          # text=paste(colnames(incidMat), row.names(incidMat)),
          type = "heatmap")
  return(p)
}
