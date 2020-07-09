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
      dashboardHeader(title = "scATAC of the Fly brain development",
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
          menuItem("Experimental design", tabName = "ExperimentalDesign", icon = icon("flask", lib="font-awesome")),
          # menuItem("The datasets", tabName = "nothing", icon = icon("seedling", lib="font-awesome"),
          #          menuSubItem("Overview", tabName = "main"),
          #          menuSubItem("Adult brain scATAC", tabName = "MenuItem1-sub1"),
          #          menuSubItem("Larval brain scATAC", tabName = "MenuItem1-sub4"),
          #          menuSubItem("scATAC across development", tabName = "MenuItem1-sub3")),
          hr(style="height:1px; width=80%; border-width:0; background-color:#204045"),
          menuSubItem("Cell types", tabName = "CellTypes", icon = icon("brain", lib="font-awesome")), # brain
          menuSubItem("Regulatory networks", tabName = "Nw_CellTypeTFs", icon = icon("project-diagram", lib="font-awesome")), # brain
          menuSubItem("Network tables", tabName = "Nw_Tables", icon = icon("table", lib="font-awesome")),
          menuItem("Development ??", tabName = "Nw_Devel", icon = icon("code-branch", lib="font-awesome")), # egg does not work
          menuSubItem("Enhancers tested", tabName = "enhancersTested", icon = icon("microscope", lib="font-awesome")),
          hr(style="height:1px; width=80%; border-width:0; background-color:#204045"),
          menuItem("Stats & Facts", tabName = "Stats", icon = icon("bar-chart", lib="font-awesome")),
          # menuItem("FAQ", tabName = "MenuItemFAQ", icon = icon("question-circle", lib="font-awesome")),
          menuItem("Data downloads", tabName = "Downloads", icon = icon("cloud-download-alt", lib="font-awesome")),
          menuItem("Tutorial", tabName = "MenuItemTutorial", icon = icon("youtube", lib="font-awesome")),
          menuItem("About us", tabName = "MenuItemAbout", icon = icon("user", lib="font-awesome")),
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

          tabItem(tabName = "ExperimentalDesign",
                  # img(src="img/experiment.png", width="60%"),
                  page_datasetExpDesign <- fluidPage(
                    includeMarkdown("md/introToDataset_main.Rmd")
                  )
          ),
          
          tabItem(tabName = "CellTypes",
                  tabsetPanel(type = "tabs",
                              id = "CellTypes-tab",
                              
                              # Tab:
                              tabPanel("Cell type info -",
                                       value='ClInfo',
                                       includeMarkdown("md/cellTypes.Rmd")
                                       # tableLoad.ui("tbl_ClInfo")
                              ),
                              
                              # Tab:
                              tabPanel("Overview (DR)",
                                       value = "3D",
                                       page_tsne3D
                              ),
                              
                              # Tab:
                              tabPanel("Topics",
                                       value = "topicsPerCellType",
                                       includeMarkdown("md/topics.Rmd"),
                                       page_topics
                              ),
                              
                              # Tab:
                              tabPanel("OtherFiguresAvailable",
                                       value = "other",
                                       includeMarkdown("md/figuresAvailable.Rmd")
                              )
                  )
          ),

          tabItem(tabName = "Nw_CellTypeTFs",
                  tabsetPanel(type = "tabs",
                              id = "Nw_CellTypeTFs-tab",
                              
                              # Tab:
                              tabPanel("Regulatory networks",
                                       value = "",
                                       includeMarkdown("md/regulatoryNetworks_explanation.Rmd"),
                                       sampleNetwork.ui("tab_networkExample")
                              ),
                              
                              # Tab:
                              tabPanel("TFs per cell type",
                                       value = "dotplots",
                                       includeMarkdown("md/tfsCellType_dotplot.Rmd"),
                                       page_tfsCellType_dotplot
                              ),

                              # Tab:
                              tabPanel("TF details",
                                       value = "tfDetails",
                                       page_tfsCellTypeDetails,
                              ),
                              
                              # Tab:
                              tabPanel("Cistrome overlaps",
                                       value = "regionsHeatmap",
                                       page_tfsCellType_regionsIntersections
                                       
                              )
                  )
          ),

          tabItem(tabName = "Nw_Tables",
                  tabsetPanel(type = "tabs",
                              id = "networks_tables",

                              tabPanel("Network tables",
                                       value = "",
                                       includeMarkdown("md/regulatoryNetworks_tables.Rmd")
                              ),

                              # tabPanel("[Visualization]",
                              #          value = "Network",
                              #          ),

                              tabPanel("Query by region -",
                                       value = "Query",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_regionInfo.Rmd"), #  TODO: merge with tableLoad.ui("tbl_RegionInfo")
                                       query_byRegion.ui("tbl_regionQueryOutput")
                              ),

                              tabPanel("DARs",
                                       value="DARs",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
                                       tableLoad.ui("tbl_DARs")
                              ),

                              tabPanel("Motif enrichment",
                                       value="nw_motifEnrichment",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_motifEnrichment.Rmd"),
                                       page_nw_tblsMotifEnrichment
                              ),

                              tabPanel("TFs per cell type -",
                                       value="TfsPerCellType",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_tfsPerCellType.Rmd"),
                                       tableLoad.ui("tbl_TfsPerCellType")
                              ),

                              tabPanel("Regions per motif -",
                                       value="SignifRegions",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                                       tableLoad.ui("tbl_SignifRegions")
                              ),

                              tabPanel("Region-gene links -",
                                       value="Region2geneLinks",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_region2geneLinks.Rmd"),
                                       tableLoad.ui("tbl_Region2geneLinks")
                              ),
                              
                              tabPanel("RNA markers",
                                       value="nw_RNAmarkers",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_RNAmarkers.Rmd"),
                                       page_nw_tblsRNA
                              )
                  )
          ),
          
          tabItem(tabName = "Nw_Devel",
                  "TO DO"
          ),
          
          tabItem(tabName = "enhancersTested",
                  includeMarkdown("md/enhancers.Rmd"),
          ),

          tabItem(tabName = "Stats",
                  "TO DO"
          ),
          
          tabItem(tabName = "Downloads",
                  tabsetPanel(type = "tabs",
                              id = "downloads-tab",

                              tabPanel("Resources",
                                       includeMarkdown("md/resources.Rmd")
                              ),

                              tabPanel("Cell Info",
                                       value='CellInfo',
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_cellInfo.Rmd"),
                                       tableLoad.ui("tbl_CellInfo")
                              )
                  )
          ),

          tabItem(tabName = "MenuItemTutorial",
                  includeMarkdown("md/tutorial.Rmd")
          ), 
          tabItem(tabName = "MenuItemAbout",
                  includeMarkdown("md/aboutUs.Rmd")
          )
          )# Tabitems
          
                              ### Other data available
                              # Janelia images / Braincode?
                              # AUCell viewer?
                              
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