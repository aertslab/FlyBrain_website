nMax <- 10

source('libs/convertToDbRegions.R')
source('libs/keepUniquePairs.R')
signifRegions <- arrow::read_feather(file="/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/03_signifRegions.lvl1p_Auc0105_Motifs.feather", mmap = TRUE)
enrichmentTable <- readRDS("../data/tbl_motifEnr_DARs.Rds"); nrow(enrichmentTable)
enrichmentTable <- enrichmentTable %>%
  dplyr::filter(me_rocThr %in% c("auc03", "auc01", "auc05")) %>%      # discards: auc005 auc001 auc10
  dplyr::filter(me_DB %in% c("motifs"))      # discards ChIP
nrow(enrichmentTable)
# High-confidence TF annotation 
tfAnnot <- RcisTarget::importAnnotations("/staging/leuven/res_00001/databases/cistarget/motif2tf/motifs-v9-nr.flybase-m0.001-o0.0.tbl"); nrow(tfAnnot)
tfAnnot <- tfAnnot[apply(tfAnnot[,c("directAnnotation", "inferred_Orthology")], 1, any),]; nrow(tfAnnot)
tfAnnot <- tfAnnot[,c("motif", "TF")]

dars <- readRDS("../data/tbl_DARs.Rds"); nrow(dars)
dars <- dars %>%
  dplyr::filter(clResolution %in% c("lvl1", "majorTypes", "merged")) %>%
  distinct()
nrow(dars)
cistromes <- readRDS("../data/cistromes.Rds")$DARs; nrow(cistromes)
# cistromes <- cistromes[which(cistromes$cellType %in% as.character(unique(unlist(dars$cellType)))),]; nrow(cistromes)

load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/02_rna_genesDetected_perc.RData")
genesDetectedPerc <- lapply(genesDetectedPerc, function(x) names(x)[x>=0.10]) 

# # Region-Gene (50kbp)
# load("int/0_genesPer_RegionPlusPeaks_50kbp_v1.0.RData")
# load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_resources/dm6_ctxRegionsAroundGenes/50kbpUp50kbpDw/genesPerCtxRegion_v1.0.RData")
# genesPerRegion <- c(genesPerRegion, genesPerCtxRegion); rm(genesPerCtxRegion)

# Region-gene (link)
load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20191109_enhancer2gene/20200722_adult_ctx/int/region2geneLinks.df.RData")#; head(region2geneLinks)
region2geneLinks_ctx <- region2geneLinks; nrow(region2geneLinks_ctx); rm(region2geneLinks)
load("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20191109_enhancer2gene/20200722_adult_peaks/int/region2geneLinks.df.RData")#; head(region2geneLinks)
region2geneLinks <- rbind(region2geneLinks, region2geneLinks_ctx); nrow(region2geneLinks); rm(region2geneLinks_ctx)
region2geneLinks <- unique(region2geneLinks[,c("regRegion", "targetGene", "rankScore", "Genie3_weight", "corr")])
region2geneLinks <- region2geneLinks[which(region2geneLinks$rankScore >= 600),]  # PARAMETER?

regions <- "X:18552058-18553789
3L:9525431-9526414
2R:10442344-10442886
2L:828683-829045
2L:20050798-20051699
3R:11450825-11451124
X:534838-535834
3L:11172605-11174212
X:18554917-18555296
3R:27030417-27031028
3L:21594210-21595125
3R:16286179-16286811
2R:16914691-16915429
2L:9841845-9842760
2L:8294572-8296098
2R:9760598-9760898
3R:20696199-20696602
3L:9193235-9193582
2L:20040501-20041072
2L:13035244-13036353
3R:24750047-24750419
3R:26711114-26711413
2R:16905794-16906591
3R:21945795-21946260
2R:22589191-22589701
3R:20520001-20520440
2R:22593249-22593657
2L:3442142-3442910
2L:2940865-2942344
X:2559674-2560058
3R:20518577-20519940
X:18193321-18193645
2L:13036762-13037773
3L:4613755-4614075
3R:24738946-24740045
3L:9193219-9194041
2R:22405484-22405990
2L:15024300-15024850
3L:8865044-8865572
2L:12152489-12153528
2L:4868253-4869348
2R:19127551-19127921
2L:19192077-19193153
3R:20696639-20697046
2L:2939983-2940863
2L:11228104-11228695
2L:3448093-3448605"
regions <- strsplit(regions, "\n")[[1]]
regions <- paste0("chr", regions)

source('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200420_WebApp/FBD_App/libs/getRegionInfo.R')
regionInfo <- getRegionInfo(queryRegions=regions,
                            signifRegions,
                            region2geneLinks,
                            genesDetectedPerc,
                            cistromes,
                            enrichmentTable,
                            dars)

DT::datatable(regionInfo, escape=F)



