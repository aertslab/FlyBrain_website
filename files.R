# ln -s /ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/data ./
setwd("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200420_WebApp/FBD_App")
library(data.table)

##### ..... June 2020 ....... ----- 
# (careful if overwritting files, the "backup app" uses the files "Up to May 2020")

## Celltype - Topics tab ----
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/04_topicsExploring_lvl10.4.1/TopicRegionTypes_forWebsite.Rds",
          "../data/TopicRegionTypes_hm.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/04_topicsExploring_lvl10.4.1/TopicCellTypes_forWebsite.Rds",
          "../data/TopicCellTypes_hm.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/04_topicsExploring_lvl10.4.1/RSS_forWebsite.Rds",
          "../data/TopicRSS.Rds", overwrite = T)

### TFs tab ----

### Cistrome intersections
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/2_exploring/aux_plotRegionHeatmap.R",
          "libs/cistromes_plotRegionHeatmap.R", overwrite = T)

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/2_exploring/ctOrder.RData",
          "../data/cistromes_ctOrder.RData", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/2_cistromeOverlaps/cistromes.Rds",
          "../data/cistromes.Rds", overwrite = T)

### Dotplot 
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/dotplotsList.RData",
          "../data/dotplotsList.RData", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/dotplotsList_tfOrder.RData",
          "../data/dotplots_tfOrder.RData", overwrite = T)

### TF details
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/05_meanExprNes.Rds",
          "../data/TFsDetail_meanExprNes.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/05_meanExprNes_Tfs.Rds",
          "../data/TFsDetail_meanExprNes_Tfs.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/05_meanAcc_cistromeByCell.mat.splitCistromes.Rds",
          "../data/TFsDetail_meanAcc_cistromeByCell.mat.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/05_meanAcc_cistromeByType.df.splitCistromes.Rds",
          "../data/TFsDetail_meanAcc_cistromeByType.df.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/cistrome_binding_sites/motifsPerTf_orderedByNes.Rds",
          "../data/TFsDetail_motifsPerTf_orderedByNes.Rds", overwrite = T)
# Network/tables ----
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/02_RNAmarkers.Rds",
          "../data/tbl_RNAmarkers.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/02_rna_genesDetected.df.Rds",
          "../data/tbl_RNAgenesDetected.Rds", overwrite = T)

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/01_darsMerged-Formatted.Rds",
          "../data/tbl_DARs.Rds", overwrite = T)

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/int/03_enrichmentTable_2_wTFs.Rds",
          "../data/tbl_DARs_motifEnr_Full.Rds", overwrite = T) # not used, but it would be for the downloads
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/03_enrichmentTable_2_fewerColumns_auc0105.Rds",
          "../data/tbl_motifEnr_DARs.Rds", overwrite = T)
# met <- readRDS("../data/tbl_DARs_motifEnr.Rds")
# met <- data.frame(met); nrow(met)
# met <- met[which(met$me_rocThr %in% "auc01"),]; nrow(met)
# met <- met[which(met$resolution %in% c("lvl1",  "majorTypes", "merged")),]; nrow(met)  # discards lvl2
# source('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/cisTopic/runRcisTarget_dm6_withBg.R')
# met <- keepUniquePairs(met, col1="cellType") # takes long!!!
# # 494135 to 187741
# saveRDS(met, file=paste0("../data/tbl_motifEnr_DARs_auc01_simplified.Rds"))

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/B_peaks/int/03_enrichmentTable_2_fewerColumns_auc0105.Rds",
          "../data/tbl_motifEnr_peaks.Rds", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/C_topics/int/03_enrichmentTable_2_fewerColumns_auc0105.Rds",
          "../data/tbl_motifEnr_topics.Rds", overwrite = T)

# Aux ----
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/int_200topics/drList.RData",
          "../data/drList_adultPupa.RData", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.4/files/cellData_0.4.1.RData",
          "../data/cellData_0.4.1.RData", overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.4/files/colVars_0.4.1.RData",
          "../data/colVars_0.4.1.RData", overwrite = T)

##### ..... Up to May 2020 ..... ----- 
setwd("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200420_WebApp/data")
### TFs tab
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/tfs.Rds", "TFsDetail_tfs.Rds",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/accessibilityMat.df.Rds","TFsDetail_accessibilityMat.df.Rds",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/nesMat.Rds","TFsDetail_nesMat.Rds",overwrite = T)
# file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.3/colVars_0.3.2.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/cellTypeColVar.RData", "TFsDetail_cellTypeColVar.RData",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/meanExprNesMat.df.Rds", "TFsDetail_meanExprNesMat.df.Rds",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/meanAccPerTypeMat.df.Rds", "TFsDetail_meanAccPerTypeMat.df.Rds",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/tfs.Rds", "TFsDetail_tfs.Rds",overwrite = T)


### Dotplot tab
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/data/dotPlot_highConfAnnot_UPdars.p.RData", "TFsDotplots_annot_hc.RData",overwrite = T)
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/data/dotPlot_bothAnnots.p.RData", "TFsDotplots_annot_both.RData",overwrite = T)


### query tab
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_anyAnnot/darRegions.RData", ".") 

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_highConfAnnot/byCellType_regionsPerTF.RData", ".") # ~regulon
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_highConfAnnot/byCellType_signifRegions_genesNearby.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_anyAnnot/genesDetected_in10perc_CellTypeGroups.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_anyAnnot/atacClsList.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/tmp/regionToMotif_noBg.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/tmp/regionToMotif_withBg.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_resources/dm6_ctxRegionsAroundGenes/5kbpUpPlusTr/ctxRegionsPerGene_v1.0.RData","ctxRegionsPerGene_v1.0_5kbp.RData")

# From global TF-cellType analysis : 
# /ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global

### Cell info ----
cto <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191219_brainDevel_warpLDA/int_230topics/cisTopicObject_4_rAnnot.Rds")
cellData <- cto@cell.data
saveRDS(cellData, file="cellInfo.Rds")

# Region info
regionInfo <- cto@region.data
regionInfo <- regionInfo[,grep("Scores_Topic", colnames(regionInfo), invert=T)]
dim(regionInfo)
saveRDS(regionInfo, file="regionInfo.Rds")

head(regionInfo)

### Cell-type info ----
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.3/clusterInfo_0.3.4.RData", ".")
objectName <- load("clusterInfo_0.3.4.RData")
sst <- eval(as.name(objectName))
rm(list=objectName)

### Markers RNA ----
load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_anyAnnot/markersRNA.RData")
sst <- markersRNA

rownames(sst) <- NULL
colnames(sst)
for(cl in c("p_val","avg_logFC","pct.1", "pct.2", "p_val_adj", "pct.ratio", "nCellTypesPos")){
  if(cl %in% colnames(sst)) sst[,cl] <- signif(as.numeric(sst[,cl]),2)
}
for(cl in c("cluster")){
  if(cl %in% colnames(sst)) sst[,cl] <- as.factor(as.character(sst[,cl]))
}
DT::datatable(sst[which(rownames(sst) %in% sample(rownames(sst), 100)),], filter="top")
saveRDS(sst, file="markersRNA.Rds")

### Markers ATAC ----
dar <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/04_Markers/Seurat_markers_AdultClusters/seurat_AdultClusters_markers_cellType_wGenes.Rds")

darBranches <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/04_Markers/Seurat_markers_BranchesAdult_4/seurat_BranchMarkers_markers_clType_wGenes.Rds")

sst <- rbind(dar, darBranches)
dim(sst)
head(sst)

rownames(sst) <- NULL
colnames(sst)
for(cl in c("p_val","avg_logFC","pct.1", "pct.2", "p_val_adj", "pct.ratio", "nCellTypesPos")){
  if(cl %in% colnames(sst)) sst[,cl] <- signif(as.numeric(sst[,cl]),2)
}
for(cl in c("cluster")){
  if(cl %in% colnames(sst)) sst[,cl] <- as.factor(as.character(sst[,cl]))
}
DT::datatable(sst[which(rownames(sst) %in% sample(rownames(sst), 100)),], filter="top")
saveRDS(sst, file="DARs_adult.Rds")


### Motif enrichment DARs adult ----
# To do: include larva clusters

## Full table
# load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/motifEnrichment_DAR_anyAnnot/enrichmentTable_merged.RData")
load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/motifEnrichment_DAR_highConfAnnot/enrichmentTable_merged_HighConfTFs.RData")
dim(enrichmentTable)
# 332885     15
head(enrichmentTable)
table2show$darSet <- table2show$geneSet
enrichmentTable$darSet <- factor(enrichmentTable$geneSet)
enrichmentTable$geneSet <- factor(enrichmentTable$geneSet)
enrichmentTable$motif <- factor(enrichmentTable$motif)

table2show <- enrichmentTable
table2show <- RcisTarget::addLogo(table2show, motifCol="motif", dbVersion="v9dl")
colsToShow <- c("logo", "NES", "TF_marker","TF_expressed","TF_annot", "cellType","DAR_type","settings_DB","settings_bg","settings_aucThr","motif","TFisExpressed","TFisMarker",  "TF_annot_1direct", "TF_annot_2ort", "TF_annot_3simil","TF_annot_HighConf")
colnames(table2show)[which(!colnames(table2show) %in% colsToShow)]
table2show <- table2show[,c("logo", "NES", "TF_marker","TF_expressed","TF_annot", "cellType","DAR_type","settings_DB","settings_bg","settings_aucThr","motif","TFisExpressed","TFisMarker",  "TF_annot_1direct", "TF_annot_2ort", "TF_annot_3simil","TF_annot_HighConf")]
# y <- DT::datatable(table2show, escape=FALSE, filter="top", options=list(pageLength=100))#; y
saveRDS(table2show, file="darsMotifEnrichment.Rds")

library(DT)
table2show <- as.data.frame(enrichmentTable); nrow(table2show)
table2show <- table2show[grep("@ auc01", table2show$settings),]; nrow(table2show)
source('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20190708_adultFlyBrain/auxScripts/runRcisTarget_dm6_withBg.R')
table2show <- keepUniquePairs(table2show, col1="geneSet")
# 
table2show <- RcisTarget::addLogo(table2show, motifCol="motif", dbVersion="v9dl")
colsToShow <- c("logo", "NES", "TF_marker","TF_expressed","TF_annot", "cellType","DAR_type","settings_DB","settings_bg","settings_aucThr","motif","TFisExpressed","TFisMarker",  "TF_annot_1direct", "TF_annot_2ort", "TF_annot_3simil","TF_annot_HighConf")
colnames(table2show)[which(!colnames(table2show) %in% colsToShow)]
table2show <- table2show[,c("logo", "NES", "TF_marker","TF_expressed","TF_annot", "cellType","DAR_type","settings_DB","settings_bg","settings_aucThr","motif","TFisExpressed","TFisMarker",  "TF_annot_1direct", "TF_annot_2ort", "TF_annot_3simil","TF_annot_HighConf")]
saveRDS(table2show, file=paste0("darsMotifEnrichment_auc01_simplified.Rds"))
# y <- DT::datatable(table2show[1:1000,], escape=FALSE, filter="top", options=list(pageLength=100))#; 


## Genes detected in RNA per cell type
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/int_anyAnnot/genesDetectedPerc.Rds", ".")

## TFs with expression and motif
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/TFs_perCellType.Rds", ".")

## Enhancer-gene links:
load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20191109_enhancer2gene/20200107_adultOnly_highConf/int2/region2geneLinks.df.RData")
region2geneLinks$TSS <- paste0(region2geneLinks$tss.seqnames, ":", region2geneLinks$tss.start, "-", region2geneLinks$tss.end)
colnames(region2geneLinks)[which(colnames(region2geneLinks)=="regRegion")] <- "Regulatory region"
colnames(region2geneLinks)[which(colnames(region2geneLinks)=="maxVal")] <- "MaxAcc"
colnames(region2geneLinks)[which(colnames(region2geneLinks)=="targetGene")] <- "Gene"
colnames(region2geneLinks)[which(colnames(region2geneLinks)=="rankScore")] <- "LinkScore"

region2geneLinks <- region2geneLinks[,which(!colnames(region2geneLinks) %in% c("tss.seqnames", "tss.start", "tss.end", "tss.GENENAME", "tss.width","genie3rankGlobal","genie3rankBin", "corrDir"))]
region2geneLinks <- region2geneLinks[,c("Regulatory region", "MaxAcc", "Gene", "TSS", "tss.strand", "LinkScore", "Genie3_weight", "genie3rankBinQ", "corr")]
head(region2geneLinks)
# rankScore <- accRank + corrRank + genie3rankBinQ
nrow(region2geneLinks)
rownames(region2geneLinks) <- NULL 
saveRDS(region2geneLinks, file="region2geneLinks.Rds")

### Signif regions per motif 

# signifRegions <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/motifEnrichment_DAR_anyAnnot/signifRegions.Rds")
# # signifRegions2 <- readRDS("motifEnrichment_DAR_highConfAnnot/signifRegions.Rds") # all are included in anyAnnot
# # signifRegions <- unique(rbind(signifRegions, signifRegions2))
# nrow(signifRegions)

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/1_global/motifEnrichment_DAR_anyAnnot/signifRegions.Rds", ".")

file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20200116_develByPieces_upto12h_200topics/02_dr_cells/umap00_3D_larvaPupaCells.RData",".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/deprecated/ATAC_v0.2/versions/cellData_0.2.1.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/deprecated/ATAC_v0.2/versions/colVars_0.2.1_merged.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20200116_develByPieces_upto12h_200topics/03_clusters_LouvainTopics_200t/upto12h_900fip__05k_5eps_55clusters.RData", ".")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/2_TF_heatmap_highConfAnnot/int/meanExprMat.df__highConfAnnot_UPdars.RData", ".")
load("meanExprMat.df__highConfAnnot_UPdars.RData")
source("libs/dotheatmap.R")
p <- dotheatmap(enrichmentDf=meanExprMat.df,
                var.x="gene", var.y="cellType",
                var.col="expression",
                col.low="lightgrey", col.mid="floralwhite", col.high="red",
                var.size="NES", min.size=.2, max.size=5)
save(p, file="dotPlot_highConfAnnot_UPdars.p.RData")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/2_TF_heatmap_MergedAnnots/dotPlot_merged.p.RData", "dotPlot_bothAnnots.p.RData")


### Topic enrichment --- 
topics_chipEnrichment <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/int_200topics/topics_chipEnrichment.Rds")$motifEnrichmentTable
topics_chipEnrichment$settings <- "ChIP"
topics_motifEnrichment_aucMax01 <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/int_200topics/topics_motifEnrichment_aucMax01.Rds")$motifEnrichmentTable
topics_motifEnrichment_aucMax01$settings <- "noBg @ auc01"
topics_motifEnrichment_withBackground <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/int_200topics/topics_motifEnrichment_withBackground.Rds")$motifEnrichmentTable
topics_motifEnrichment_withBackground$settings <- "withBg"

head(topics_chipEnrichment)
head(topics_motifEnrichment_aucMax01)
head(topics_motifEnrichment_withBackground)
colnames(topics_chipEnrichment)[which(colnames(topics_chipEnrichment) == "accession")] <- "motif"
met <- rbind(topics_chipEnrichment, topics_motifEnrichment_aucMax01, topics_motifEnrichment_withBackground)

table2show <- met
table2show <- table2show[order(table2show$NES, decreasing=T),]
table2show <- RcisTarget::addLogo(table2show, motifCol="motif", dbVersion="v9dl")
saveRDS(table2show, file="topicsAdultMotifEnrichment.Rds")



### libs ----
file.copy('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/dotheatmap.R', "libs/.")
file.copy("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/col_vector.R", "libs/.")
file.copy('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/cisTarget/convertToDbRegions.R', "libs/.")
file.copy('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/cisTopic/keepUniquePairs.R', "libs/.")
file.copy('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/cisTopic/plotCont.R', "libs/.")


