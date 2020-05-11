####
# For each pair Topic-Motif keep best threshold (NES)
keepUniquePairs <- function(met, col1="topic", col2="motif", colsToKeep=colnames(met), verbose=TRUE)
{
  met[,col1] <- as.character(met[,col1])
  
  met$uniqueCombos <- paste(met[,col1], met[,col2], sep=" <<===>> ")
  uniqueCombos <- unique(met$uniqueCombos)
  
  table2show <- data.table::rbindlist(lapply(uniqueCombos, function(x){
    x <- met[which(met$uniqueCombos %in% x),]
    x$n <- nrow(x)
    x[which.max(x$NES),,drop=FALSE]
  }))
  table2show$NES <- as.numeric(as.character(table2show$NES))
  table2show <- table2show[, ..colsToKeep]
  
  if(verbose) message("from ", nrow(met), " to ", nrow(table2show))
  invisible(table2show)
}

####
# 
simplifyByTF <- function(met, simplifyCol="cellType", motifCol="motif", tfsCol="TF_expressed")
{
  allSets <- unique(as.character(unlist(met[,simplifyCol,with=F]))); length(allSets)
  
  tables2show <- list()
  for(setName in allSets)
  {
    metSubset <- met[which(met[,simplifyCol,with=F]==setName),]
    allMotifs <- unique(as.character(unlist(metSubset[,motifCol,with=F]))); length(allMotifs)
    if(nrow(metSubset) != length(allMotifs)) stop("The input table should already be simplified by 'simplifyCol'")
  
    tfsList <- setNames(unlist(as.list(unname(metSubset[,tfsCol,with=F])), recursive=F), 
                        as.character(unlist(metSubset[,motifCol,with=F])))
    tfsList <- lapply(tfsList, function(x) strsplit(x, "; ")[[1]])
    tfs.df <- reshape2::melt(tfsList)
    colnames(tfs.df) <- c("tf", "motif")
    allTFs <- unique(as.character(tfs.df[,"tf"])); length(allTFs)
    tables2show[[setName]] <- data.table::rbindlist(lapply(allTFs, function(tf){
      # message(tf)
      motifs <- unique(tfs.df[which(tfs.df==tf),"motif"])
      x <- metSubset[which(unlist(metSubset[,motifCol,with=F]) %in% motifs),]
      x <- x[which.max(x$NES),,drop=FALSE]
      
      cbind(TF=tf, x[,c(simplifyCol, motifCol, "NES"), with=F])
      }))
    rm(allMotifs)
    rm(allTFs)
  }
  table2show <- data.table::rbindlist(tables2show)
  invisible(table2show)
}

