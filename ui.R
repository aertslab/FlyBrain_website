if(grepl("r23", system("hostname",intern=T))) .libPaths("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/R/x86_64-pc-linux-gnu-library/3.6.0_genius")
if(!grepl("r23", system("hostname",intern=T))) .libPaths("/media/seq-srv-06/lcb/saibar/R_libs/x86_64-pc-linux-gnu-library/3.6")
print(.libPaths())

for(pkg in c("shinycssloaders","plotly","visNetwork","DT", "shinydashboard")) if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
for(pkg in c("BiocParallel","RcisTarget","GenomicRanges")) if(!pkg %in% rownames(installed.packages())) BiocManager::install(pkg)

library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(arrow)

## To avoid a huge file, each page is described in an indepentent file: 
source("page_tsne3D_ui.R") # first...
pagesToLoad <- list.files(pattern = "page_*")
pagesToLoad <- grep("^- ", pagesToLoad, invert=T, value=T) # ignore those starting by "- "
invisible(sapply(pagesToLoad, source))

## UI  ----
ui <- function(request) {
  # shinyUI(
    dashboardPage(
      skin="black",
      ## Header  ----
      dashboardHeader(title = "Fly Brain",
                      # tags$li(class = "dropdown", bookmarkButton(label="Link to this view")),
                      disable=FALSE
                      ),
      ## Sidebar  ---- 
      # List of links to include in the sidebar
      # https://rstudio.github.io/shinydashboard/behavior.html
      dashboardSidebar(
        sidebarMenu(id="page", text="test", icon="brain", 
          # https://fontawesome.com/icons ; https://icons.getbootstrap.com/ 
          menuItem("Home", tabName = "HomePage", icon = icon("home", lib="font-awesome")), 
          # menuItem("Experimental design", tabName = "ExperimentalDesign", icon = icon("flask", lib="font-awesome")),
          # menuItem("The datasets", tabName = "nothing", icon = icon("seedling", lib="font-awesome"),
          #          menuSubItem("Overview", tabName = "main"),
          #          menuSubItem("Adult brain scATAC", tabName = "MenuItem1-sub1"),
          #          menuSubItem("Larval brain scATAC", tabName = "MenuItem1-sub4"),
          #          menuSubItem("scATAC across development", tabName = "MenuItem1-sub3")),
          hr(style="height:1px; width=80%; border-width:0; background-color:#204045"),
            #menuItem("Cell types ??", tabName = "CellTypes", icon = icon("brain", lib="font-awesome")), # brain
            menuItem("Regulatory networks", tabName = "eGRNs", icon = icon("project-diagram", lib="font-awesome")), # brain
            menuItem("Network tables", tabName = "NwTables", icon = icon("table", lib="font-awesome")),
            menuItem("Enhancer architecture", tabName = "enhancerArchitecture", icon = icon("microscope", lib="font-awesome")),
            menuItem("Stats", tabName = "Stats", icon = icon("bar-chart", lib="font-awesome")),
          #hr(style="height:1px; width=80%; border-width:0; background-color:#204045"),
            #menuItem("Development ??", tabName = "Nw_Devel", icon = icon("code-branch", lib="font-awesome")), # egg does not work
          hr(style="height:1px; width=80%; border-width:0; background-color:#204045"),
          menuItem("Data downloads", tabName = "Downloads", icon = icon("cloud-download-alt", lib="font-awesome")),
          menuItem("VideoAbstract", tabName = "VideoAbstract", icon = icon("youtube", lib="font-awesome")),
          menuItem("Tutorial", tabName = "Tutorial", icon = icon("question-circle", lib="font-awesome")),
          menuItem("About us", tabName = "About", icon = icon("user", lib="font-awesome")),
          hr(style="height:2px; width=80%; border-width:0; background-color:#90a0a050"),
          menuItem("SCope",  href="http://scope.aertslab.org/#/Fly_Brain/", icon = icon("brain", lib="font-awesome")))
      ), 
      
      ## Body  ----
      # Tabs within pages need to be defined here, otherwise they cannot be bookmarked
      dashboardBody(
        tabItems(

          tabItem(tabName = "HomePage",
                  page_home
          ),

          # tabItem(tabName = "ExperimentalDesign",
          #         # img(src="img/experiment.png", width="60%"),
          #         page_datasetExpDesign <- fluidPage(
          #           includeMarkdown("md/introToDataset_main.Rmd")
          #         )
          # ),
          
          tabItem(tabName = "CellTypes",
                  " Under construction "
                  # tabsetPanel(type = "tabs",
                  #             id = "CellTypes-tab",
                  # 
                  #             # Tab:
                  #             tabPanel("Cell type info -",
                  #                      value='ClInfo',
                  #                      includeMarkdown("md/cellTypes.Rmd")
                  #                      # tableLoad.ui("tbl_ClInfo")
                  #             ),
                  # 
                  #             # Tab:
                  #             tabPanel("Overview (DR)",
                  #                      value = "3D",
                  #                      page_tsne3D
                  #             ),
                  # 
                  #             # Tab:
                  #             tabPanel("Topics",
                  #                      value = "topicsPerCellType",
                  #                      includeMarkdown("md/topics.Rmd"),
                  #                      page_topics
                  #             )
                  # )
          ),

          tabItem(tabName = "eGRNs",
                  tabsetPanel(type = "tabs",
                              id = "eGRNs-tab",
                              
                              # Tab:
                              tabPanel("Regulatory networks",
                                       value = "",
                                       includeMarkdown("md/eGRN_index.Rmd")
                              ),
                              
                              # Tab:
                              tabPanel("TFs per cell type",
                                       value = "dotplots",
                                       includeMarkdown("md/eGRN_dotplot.Rmd"),
                                       page_eGRN_dotplot
                              ),

                              # Tab:
                              tabPanel("TF details",
                                       value = "tfDetails",
                                       includeMarkdown("md/eGRN_details.Rmd"),
                                       page_eGRN_tfDetails
                              ),
                              
                              # Tab:
                              tabPanel("Cistrome overlaps",
                                       value = "regionsHeatmap",
                                       includeMarkdown("md/eGRN_cistromeOverlaps.Rmd"),
                                       page_eGRN_regionsIntersections
                                       
                              )
                  )
          ),

          tabItem(tabName = "NwTables",
                  tabsetPanel(type = "tabs",
                              id = "networks_tables",

                              tabPanel("Data tables",
                                       value = "",
                                       includeMarkdown("md/tables_info.Rmd")
                              ),

                              tabPanel("Networks",
                                       value="tab_nw",
                                       includeMarkdown("md/dataTablesDescr/tbl_eGRNs.Rmd"),
                                       tableLoad.ui("tbl_eGRNs")
                              ),
                              
                              tabPanel("DARs",
                                       value="tab_DARs",
                                       includeMarkdown("md/dataTablesDescr/tbl_DAR.Rmd"),
                                       tableLoad.ui("tbl_DARs")
                              ),

                              tabPanel("Motif enrichment",
                                       value="tab_motifEnrichment",
                                       includeMarkdown("md/dataTablesDescr/tbl_motifEnrichment.Rmd"),
                                       page_tblsMotifEnrichment
                              ),

                              tabPanel("TFs per cell type",
                                       value="tab_TfsPerCellType",
                                       includeMarkdown("md/dataTablesDescr/tbl_tfsPerCellType.Rmd"),
                                       tableLoad.ui("tbl_TfsPerCellType")
                              ),

                              tabPanel("Regions per motif",
                                       value="tab_RegionsPerMotif",
                                       includeMarkdown("md/dataTablesDescr/tbl_RegionsPerMotif.Rmd"),
                                       tableLoad.ui("tbl_RegionsPerMotif")
                              ),

                              tabPanel("Region-gene links",
                                       value="tab_Region2geneLinks",
                                       includeMarkdown("md/dataTablesDescr/tbl_region2geneLinks.Rmd"),
                                       #tableLoad.ui("tbl_Region2geneLinks")
                                       page_tblsLinks
                              ),
                              tabPanel("RNA markers",
                                       value="tab_RNAmarkers",
                                       includeMarkdown("md/dataTablesDescr/tbl_RNAmarkers.Rmd"),
                                       page_tblsRNA
                              ),
                              #tabPanel("? Query by region",
                              #         value = "tab_Query",
                              #         includeMarkdown("md/dataTablesDescr/tbl_regionInfo_Query.Rmd"), #  TODO: merge with tableLoad.ui("tbl_RegionInfo")
                              #         query_byRegion.ui("tbl_regionQueryOutput")
                              #)
                  )
          ),
          
          tabItem(tabName = "enhancerArchitecture",
                  tabsetPanel(type = "tabs",
                              id = "enhancers_tabs",
                              
                              tabPanel("Deep learning",
                                       # value = "",
                                       includeMarkdown("md/enhancerArchitecture.Rmd"),
                                       page_enhancerArchitecture,
                                       includeMarkdown("md/enhancerArchitecture_topicLegend.Rmd")
                              ),
                              
                              tabPanel("Janelia",
                                      includeMarkdown("md/JaneliaIntersEnhs.Rmd"),
                                      #tableLoad.ui("tbl_Janelia")
                              )
                  )
          ),
                      
          tabItem(tabName = "Stats",
                  includeMarkdown("md/stats.Rmd"),
          ),
          
          tabItem(tabName = "Downloads",
                  includeMarkdown("md/DataDownloads.Rmd")
                  # tabsetPanel(type = "tabs",
                  #             id = "downloads-tab",
                  #             
                  #             tabPanel("Resources",
                  #                      includeMarkdown("md/DataDownloads.Rmd")
                  #             ),
                  # 
                  #             tabPanel("Cell Info",
                  #                      value='CellInfo',
                  #                      includeMarkdown("md/dataTablesDescr/tbl_cellInfo.Rmd"),
                  #                      tableLoad.ui("tbl_CellInfo")
                  #             )
                  # )
          ),

          tabItem(tabName = "Tutorial",
                  includeMarkdown("md/tutorial.Rmd")
          ),
          tabItem(tabName = "VideoAbstract",
                  includeMarkdown("md/videoAbstract.Rmd")
          ), 
          tabItem(tabName = "About",
                  includeMarkdown("md/aboutUs.Rmd")
          )
          )# Tabitems
                              
      )# Body
  )# Page
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
