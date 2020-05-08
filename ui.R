if(grepl("r23", system("hostname",intern=T))) .libPaths("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/R/x86_64-pc-linux-gnu-library/3.6.0_genius")
if(!grepl("r23", system("hostname",intern=T))) .libPaths("/media/seq-srv-06/lcb/saibar/R_libs/x86_64-pc-linux-gnu-library/3.6")
print(.libPaths())

for(pkg in c("shinycssloaders","plotly","visNetwork","DT", "shinydashboard")) if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
for(pkg in c("BiocParallel","RcisTarget","GenomicRanges")) if(!pkg %in% rownames(installed.packages())) BiocManager::install(pkg)

library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)

## To avoid a huge file, each page is described in an indepentent file: 
pagesToLoad <- list.files(pattern = "page_*")
pagesToLoad <- grep("^- ", pagesToLoad, invert=T, value=T) # ignore those starting by "- "
source("page_tsne3D_ui.R") # first...
invisible(sapply(pagesToLoad, source))

## UI  ----
ui <- function(request) {
  # shinyUI(
    dashboardPage(
      skin="black",
      ## Header  ----
      dashboardHeader(title = "scATAC of the Fly brain development",
                      # tags$li(class = "dropdown", bookmarkButton(label="Link to this view")),
                      disable=FALSE
                      ),
      ## Sidebar  ----
      # List of links to include in the sidebar
      # https://rstudio.github.io/shinydashboard/behavior.html
      dashboardSidebar(
        sidebarMenu(id="pages",
          # https://fontawesome.com/icons ; https://icons.getbootstrap.com/ 
          menuItem("Home", tabName = "HomePage", icon = icon("home", lib="font-awesome")), 
          menuItem("Experimental design", tabName = "MenuItemDesign", icon = icon("flask", lib="font-awesome")),
          # menuItem("The datasets", tabName = "nothing", icon = icon("seedling", lib="font-awesome"),
          #          menuSubItem("Overview", tabName = "main"),
          #          menuSubItem("Adult brain scATAC", tabName = "MenuItem1-sub1"),
          #          menuSubItem("Larval brain scATAC", tabName = "MenuItem1-sub4"),
          #          menuSubItem("scATAC across development", tabName = "MenuItem1-sub3")),
          menuItem("Adult fly brain cell types", tabName = "MenuItemAdultCellTypes", icon = icon("brain", lib="font-awesome")),
          menuItem("Cell types across development", tabName = "MenuItemDevel", icon = icon("code-branch", lib="font-awesome")), # egg does not work
          menuItem("Regulatory networks", tabName = "MenuItemNetworks", icon = icon("project-diagram", lib="font-awesome")),
          menuItem("Stats & Facts", tabName = "MenuItemStats", icon = icon("bar-chart", lib="font-awesome")),
          # menuItem("FAQ", tabName = "MenuItemFAQ", icon = icon("question-circle", lib="font-awesome")),
          menuItem("Data Downloads", tabName = "MenuItemResources", icon = icon("cloud-download-alt", lib="font-awesome")),
          menuItem("About us", tabName = "MenuItemAbout", icon = icon("user", lib="font-awesome")),
          
          hr(),
          
          menuItem("FiguresAvailable", tabName="MenuItemFigs", icon = icon("paint-brush", lib="font-awesome")),
          menuItem("TFs per cell type", tabName="MenuItemTfsCellType", icon = icon("paint-brush", lib="font-awesome")),
          menuItem("Tables available", tabName="MenuItemTables", icon = icon("table", lib="font-awesome")),
          menuItem("Query tests", tabName="MenuItemQueryTests", icon = icon("table", lib="font-awesome"))
        )
      ), 
      ## Body  ----
      # Tabs within pages need to be defined here, otherwise they cannot be bookmarked
      dashboardBody(
        tabItems(
          
          tabItem(tabName = "HomePage",
                  page_home
          ),
          
          tabItem(tabName = "MenuItemDesign",
                  img(src="img/experiment.png", width="60%"),
                  page_datasetExpDesign
          ),
          
          tabItem(tabName = "MenuItemAdultCellTypes",
                  tabsetPanel(type = "tabs",
                              id = "celltypes-tabset",
                              
                              # Tab: 
                              tabPanel("Cell type annotation on scATAC data",
                                       value = "cell-types-descr",
                                       includeMarkdown("md/adultCellTypes_overview.Rmd"),
                                       h4("scATAC tSNE"),
                                       fluidRow(
                                         img(src="img/tsne.png", width="50%")
                                       )
                                       
                              ),
                              
                              tabPanel("Central brain cell types",
                                       value = "cell-types-cb",
                                       includeMarkdown("md/adultCellTypes_neuronal.Rmd")
                              ),
                              
                              tabPanel("Optic lobe cell types",
                                       value = "cell-types-ol"
                              ),
                              
                              tabPanel("Glial cell types",
                                       value = "cell-types-glia"
                              ),
                              
                              tabPanel("DARs",
                                       value = "dars"
                              )
                  )
          ),
          
          tabItem(tabName = "MenuItemDevel",
                  # Move to external file:
                  fluidPage(
                    "To discuss with Jasper, includes larva...?",
                    img(src="img/development.png", width="80%"),
                    br(),
                    br(),
                    img(src="img/development_regions.png", width="80%"),
                  )
          ),
          
          tabItem(tabName = "MenuItemNetworks",
                  page_regulatoryNetworks
          ),
          
          tabItem(tabName = "MenuItemStats",
                  page_stats
          ),
          
          tabItem(tabName = "MenuItemResources",
                  # Move to external file:
                  page_resources,
                  fluidPage(
                    h2("another title"),
                    "- Link to .bed & .bw (ucsctracks.aertslab.org/...)"
                  )
          ),
          
          tabItem(tabName = "MenuItemAbout",
                  # Move to external file:
                  fluidPage(
                    h2("About us"),
                    br(),
                    a("Aerts lab", href="https://aertslab.org", target="_blank"), 
                    " @ ",
                    a("VIB", href="https://www.vib.be", target="_blank"),
                    " - ",
                    a("KU Leuven", href="https://www.kuleuven.be", target="_blank"),
                    br(),
                    
                    "Fly cell atlas?",
                    br(),
                    "Or enough with the footer?"
                  )
          ),
          
          tabItem(tabName = "MenuItemFigs",
                  tabsetPanel(type = "tabs",
                              id = "FiguresAvailable-tabset",
                              
                              # Tab: 
                              tabPanel("3D",
                                       value = "FiguresAvailable-tabDARs",
                                       page_tsne3D
                              ),
                              # Tab: 
                              tabPanel("TODO",
                                       value = "FiguresAvailable-tabOther",
                                       includeMarkdown("md/figuresAvailable.Rmd")
                              )
                  )
          ),
          
          tabItem(tabName = "MenuItemTfsCellType",
                  tabsetPanel(type = "tabs",
                              id = "TfsCellType-tabset",
                              
                              # Tab: 
                              tabPanel("Dotplot (high conf annot)",
                                       value = "TfsCellType-dotplot",
                                       page_tfsCellType_dotplot
                              ),
                              
                              # Tab: 
                              tabPanel("Dotplot (any annot)",
                                       value = "TfsCellType-dotplot2",
                                       "TFs appear twice: Those without sufix are the same as in the other tab ('high conf': direc & ort), the ones with '[LC]' sufix also use 'motif similarity' for Motif-TF annotation.",
                                       br(),
                                       "(If keeping both plots, move to same tab with a 'select')",
                                       page_tfsCellType_dotplot2,
                                       br(),
                                       "Note: T4/T5 are not distinguishable in the scRNA-seq dataset, so the same RNA-cluster has been taken as reference for both cell types"
                              ),
                              
                              # Tab: 
                              tabPanel("TF details",
                                       value = "TfsCellType-tfDetails",
                                       page_tfsCellTypeDetails
                                       
                              )
                  )
          ),
          
          tabItem(tabName = "MenuItemTables",
                  h2("All tables available so far:"),
                  
                  tabsetPanel(type = "tabs",
                              id = "TablesAvailable-tabset",
                              
                              # Tab:
                              tabPanel("Cell info",
                                       value="TablesAvailable-tabCellInfo",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_cellInfo.Rmd"),
                                       tableLoad.ui("tbl_cellInfo")
                              ),
                              
                              # Tab:
                              tabPanel("Cluster info",
                                       value="TablesAvailable-tabClInfo",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_clusterInfo.Rmd"),
                                       tableLoad.ui("tbl_clusterInfo")
                              ),
                              
                              # Tab:
                              tabPanel("Genes detected by cell type",
                                       value="TablesAvailable-tabGgenesDetectedPerCellType",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_genesDetectedPerCellType.Rmd"),
                                       tableLoad.ui("tbl_genesDetectedPerCellType")
                              ),
                              
                              # Tab:
                              tabPanel("RNA markers",
                                       value="TablesAvailable-tabRNAmarkers",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_RNAmarkers.Rmd"),
                                       tableLoad.ui("tbl_rnaMarkers")
                              ),
                              
                              # Tab: 
                              tabPanel("DARs",
                                       value="TablesAvailable-tabDARs",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
                                       tableLoad.ui("tbl_darCellTypes")
                              ),
                              
                              
                              # Tab:
                              tabPanel("Motif enrichment",
                                       value="TablesAvailable-tabDarsMotifEnrichment",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichment.Rmd"),
                                       tableLoad.ui("tbl_darsMotifEnrichment")
                              ),
                              
                              # Tab:
                              tabPanel("Motif enrichment (simplified)",
                                       value="TablesAvailable-tabDarsMotifEnrichmentSimpl",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_darsMotifEnrichmentSimpl.Rmd"),
                                       tableLoad.ui("tbl_darsMotifEnrichmentSimpl")
                              ),
                              
                              # Tab:
                              tabPanel("Motif enrichment (topics)",
                                       value="TablesAvailable-tabTopicsMotifEnrichment",
                                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_topicsMotifEnrichment.Rmd"),
                                       tableLoad.ui("tbl_topicsMotifEnrichment")
                              ),
                              
                              # Tab:
                              tabPanel("TFs per cell type",
                                       value="TablesAvailable-tabTfsPerCellType",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_tfsPerCellType.Rmd"),
                                       tableLoad.ui("tbl_tfsPerCellType")
                              ),
                              
                              # Tab:
                              tabPanel("Regions per motif",
                                       value="TablesAvailable-tabSignifRegions",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                                       tableLoad.ui("tbl_signifRegions")
                              ),
                              
                              # Tab:
                              tabPanel("Region info",
                                       value="TablesAvailable-tabRegionInfo",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_regionInfo.Rmd"),
                                       tableLoad.ui("tbl_regionInfo")
                              ),
                              
                              # Tab:
                              tabPanel("Region-gene links",
                                       value="TablesAvailable-tabRegion2geneLinks",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_region2geneLinks.Rmd"),
                                       tableLoad.ui("tbl_region2geneLinks")
                              )
                              
                              
                              ### Other data available
                              # Janelia images / Braincode?
                              # AUCell viewer?
                              # Topics viewer?
                              # the TF-cellType heatmap
                  )
          ),
          
          tabItem(tabName = "MenuItemQueryTests",
                  tabsetPanel(type = "tabs",
                              id = "queriesTabset",

                              # Tab:
                              tabPanel("Query",
                                       value = "Query",
                                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                                       query_byRegion.ui("tbl_regionQueryOutput")
                              ),
                              tabPanel("Network",
                                       value = "Network",
                                       sampleNetwork.ui("tab_networkExample")
                              )
                  ),
          )
        )
      )
    )
  # )
}



### doesnt work with bookmarks
# ui <- function(request) {
#   fluidPage(
#     dashboardPage(header, sidebar, body, skin="black"),
#     # Footer: Do we want one?
#     fluidRow(
#       column(12, align="center",
#              a("Aerts lab", href="https://aertslab.org", target="_blank"),
#              " @ ",
#              a("VIB", href="https://www.vib.be", target="_blank"),
#              " - ",
#              a("KU Leuven", href="https://www.kuleuven.be", target="_blank")
#       ))
#   )
# }