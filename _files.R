# ln -s /lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/data ./
setwd("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200420_Website/FlyBrain_website/")
library(data.table)
library(arrow)
dataPath <- "../data/"
dwPath <- "www/downloads/"

# Some files have been slightly reformated: setwd("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v6_eGRNs/website/")

##### ..... Aug 2021 ....... ----- 
# DeepLearning / enhancers tab ----
enhFiles <- list.files(file.path("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200420_Website/FlyBrain_website/www/", 'deepExplainer/img'), pattern=".png",recursive=T)
enhFiles <- unique(gsub(".png", "", enhFiles))
enhFiles <- sapply(strsplit(enhFiles, "/"), function(x) x[2])

enhFiles <- data.frame(fileName=enhFiles, 
                       do.call(rbind, strsplit(enhFiles, "___")),
                       stringsAsFactors=F)[,1:3]
colnames(enhFiles) <- c("fileName", "cellType", "enhancer")
enhFiles$enhancer <- sapply(strsplit(enhFiles$enhancer, "__"), function(x) x[2])
saveRDS(enhFiles, file=paste0(dataPath, file="DL_enhFiles.Rds"))


## Janelia table ----
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200110_Janelia/D_statsJaneliaIntersections/int/brainCodeRegionsLocation_6_forWebsite_janelia_IntEnhancers.feather",
          file.path(dataPath, "janelia_IntEnhancers.feather"), overwrite = T)

system("tar -czf /lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200110_Janelia/D_statsJaneliaIntersections/int/brainCodeRegionsLocation_4clean.txt.zip /lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200110_Janelia/D_statsJaneliaIntersections/int/brainCodeRegionsLocation_4clean.txt")
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200110_Janelia/D_statsJaneliaIntersections/int/brainCodeRegionsLocation_4clean.txt.zip",
          file.path(dwPath, "other/janelia_IntEnhancers.txt.zip"), overwrite = T)


## eGRNs ---- 
### Dotplot 
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v6_eGRNs/website/dotplotsList.RData",
          "dotplotsList.RData", overwrite = T)
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v6_eGRNs/website/dotplotsList_tfOrder.RData",
          "../data/dotplots_tfOrder.RData", overwrite = T)

## Network/tables ----
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v6_eGRNs/website/asFeather/tbl_region2geneLinks.feather",
          file.path(dataPath, "tbl_region2geneLinks.feather"), overwrite = T) # TODO: Download files? 

# June 2020: Don't need update (but copied again to convert to feather):
# DARs
system.time(tmp <- readRDS("/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/01_darsMerged-Formatted.Rds")); nrow(tmp)
arrow::write_feather(tmp, file.path(dataPath, "tbl_DARs.feather"))

# RNAmarkers
system.time(tmp <- readRDS("/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/02_RNAmarkers.Rds")); nrow(tmp)
arrow::write_feather(tmp, file.path(dataPath, "tbl_RNAmarkers.feather"))

system.time(tmp <- readRDS("/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v2_1_global/A_DARs/int/02_rna_genesDetected.df.Rds")); nrow(tmp)
arrow::write_feather(tmp, file.path(dataPath, "tbl_RNAgenesDetected.feather"))

        

## Downloads ----
# eGRNs (feather)
file.copy("/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200220_MotifsInEnhancers/v6_eGRNs/int/08_D.3_cistromes_eRegulons__eRegulons__correlationRanking_cistromeGenes.feather",
          file.path(dwPath, "networks/cistromes_eGRNs_Adult.feather"), overwrite = T)
        tmp <- arrow::read_feather(file.path(dwPath, "networks/cistromes_eGRNs_Adult.feather"), mmap=T)
        tmp$cellType <- factor(tmp$cellType)
        arrow::write_feather(tmp, file.path(dwPath, "networks/cistromes_eGRNs_Adult.feather"))
        
file.copy("/lustre1/project/stg_00002/lcb/jjans/analysis/development/development_atlas/Figures/Development_GRNs_final_final/0h_cistromes.feather",
          file.path(dwPath, "networks/cistromes_devel00h.feather"), overwrite = T)
file.copy("/lustre1/project/stg_00002/lcb/jjans/analysis/development/development_atlas/Figures/Development_GRNs_final_final/12h_cistromes.feather",
          file.path(dwPath, "networks/cistromes_devel12h.feather"), overwrite = T)
file.copy("/lustre1/project/stg_00002/lcb/jjans/analysis/development/development_atlas/Figures/Development_GRNs_final_final/24h_cistromes.feather",
          file.path(dwPath, "networks/cistromes_devel24h.feather"), overwrite = T)




##### ..... Jan 2021 ....... ----- 


##### ..... June 2020 ....... ----- 

## Aux ----
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/runs_cisTopic/20191216_adultPupa72_warpLDA/int_200topics/drList.RData",
          "../data/drList_adultPupa.RData", overwrite = T)
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.4/files/cellData_0.4.1.RData",
          "../data/cellData_0.4.1.RData", overwrite = T)
file.copy("/lustre1/project/stg_00002/lcb/saibar/Projects/FB_devel/Annotations/ATAC_v0.4/files/colVars_0.4.1.RData",
          "../data/colVars_0.4.1.RData", overwrite = T)
