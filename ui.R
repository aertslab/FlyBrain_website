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
        sidebarMenu(id="page",
          # https://fontawesome.com/icons ; https://icons.getbootstrap.com/ 
          menuItem("Home", tabName = "HomePage", icon = icon("home", lib="font-awesome")), 
          menuItem("Experimental design", tabName = "ExperimentalDesign", icon = icon("flask", lib="font-awesome")),
          # menuItem("The datasets", tabName = "nothing", icon = icon("seedling", lib="font-awesome"),
          #          menuSubItem("Overview", tabName = "main"),
          #          menuSubItem("Adult brain scATAC", tabName = "MenuItem1-sub1"),
          #          menuSubItem("Larval brain scATAC", tabName = "MenuItem1-sub4"),
          #          menuSubItem("scATAC across development", tabName = "MenuItem1-sub3")),
          hr(),
          menuItem("Key TFs for fly brain cell types", tabName = "CellTypes_TFs", icon = icon("brain", lib="font-awesome")),
          menuItem("Regulatory networks", tabName = "Networks", icon = icon("project-diagram", lib="font-awesome")),
          hr(),
          menuItem("Cell types across development -", tabName = "CellTypes_Devel", icon = icon("code-branch", lib="font-awesome")), # egg does not work
          menuItem("Stats & Facts", tabName = "Stats", icon = icon("bar-chart", lib="font-awesome")),
          # menuItem("FAQ", tabName = "MenuItemFAQ", icon = icon("question-circle", lib="font-awesome")),
          menuItem("Data downloads", tabName = "Downloads", icon = icon("cloud-download-alt", lib="font-awesome")),
          menuItem("About us", tabName = "MenuItemAbout", icon = icon("user", lib="font-awesome"))
          # hr(),
        )
      ), 
      
      ## Body  ----
      # Tabs within pages need to be defined here, otherwise they cannot be bookmarked
      dashboardBody(
        tabItems(

          tabItem(tabName = "HomePage",
                  page_home
          ),

          tabItem(tabName = "ExperimentalDesign",
                  img(src="img/experiment.png", width="60%"),
                  page_datasetExpDesign
          ),

          tabItem(tabName = "CellTypes_TFs",
                  tabsetPanel(type = "tabs",
                              id = "CellTypes_TFs-tab",

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
                              tabPanel("Regions intersections",
                                       value = "regionsHeatmap",
                                       page_tfsCellType_regionsIntersections
                                       
                              ),
                              
                              tabPanel("Cluster Information -",
                                       value='ClInfo',
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_clusterInfo.Rmd"),
                                       tableLoad.ui("tbl_ClInfo")
                              )
                  )
          ),

          tabItem(tabName = "CellTypes_Devel",
                  tabsetPanel(type = "tabs",
                              id = "CellTypes_Devel-tab",

                              # Tab:
                              tabPanel("3D",
                                       value = "3D",
                                       page_tsne3D
                              ),
                              # Tab:
                              tabPanel("TODO",
                                       value = "other",
                                       includeMarkdown("md/figuresAvailable.Rmd")
                              )
                  )
          ),

          tabItem(tabName = "Networks",
                  tabsetPanel(type = "tabs",
                              id = "networks_tables",

                              tabPanel("Explore regulatory networks",
                                       value = "",
                                       page_regulatoryNetworks
                              ),

                              tabPanel("Query Networks -",
                                       value = "Query",
                                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_signifRegions.Rmd"),
                                       query_byRegion.ui("tbl_regionQueryOutput")
                              ),

                              tabPanel("[Visualization]",
                                       value = "Network",
                                       sampleNetwork.ui("tab_networkExample")),

                              tabPanel("RNA markers",
                                       value="nw_RNAmarkers",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_RNAmarkers.Rmd"),
                                       page_nw_tblsRNA
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

                              tabPanel("Region info",
                                       value="RegionInfo",
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_regionInfo.Rmd"),
                                       tableLoad.ui("tbl_RegionInfo")
                              )
                  )
          ),

          tabItem(tabName = "Downloads",
                  tabsetPanel(type = "tabs",
                              id = "downloads-tab",

                              tabPanel("Resources",
                                       # Move to external file:
                                       page_resources,
                                       fluidPage(
                                         h2("Links:"),
                                         "- Link to .bed & .bw (ucsctracks.aertslab.org/...)"
                                       )
                              ),

                              tabPanel("Cell Info",
                                       value='CellInfo',
                                       includeMarkdown("md/tmp_dataTablesDescr/tbl_cellInfo.Rmd"),
                                       tableLoad.ui("tbl_CellInfo")
                              )
                  )
          ),

          tabItem(tabName = "MenuItemAbout",
                  includeMarkdown("md/aboutUs.Rmd")
          )
          )# Tabitems
          
                              ### Other data available
                              # Janelia images / Braincode?
                              # AUCell viewer?
                              # Topics viewer?
                              
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