if(grepl("r23", system("hostname",intern=T))) .libPaths("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/R/x86_64-pc-linux-gnu-library/3.6.0_genius")
if(!grepl("r23", system("hostname",intern=T))) .libPaths("/media/seq-srv-06/lcb/saibar/R_libs/x86_64-pc-linux-gnu-library/3.6")

for(pkg in c("BiocParallel","RcisTarget","GenomicRanges","shinycssloaders","plotly","visNetwork","DT", "shinydashboard")) if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)


## Header  ----
header <- dashboardHeader(title = "scATAC of the Fly brain development",
                          # tags$li(class = "dropdown", bookmarkButton(label="Link to this view")),
                          disable=FALSE)

## Sidebar  ----
# List of links to include in the sidebar
# https://rstudio.github.io/shinydashboard/behavior.html
sidebar <- dashboardSidebar(
  
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
              
              menuItem("Content examples", tabName="MenuItemExamples", icon = icon("play", lib="font-awesome")),
              menuItem("Figures available", tabName="MenuItemFigs", icon = icon("paint-brush", lib="font-awesome")),
              menuItem("TFs per cell type", tabName="MenuItemTfsCellType", icon = icon("paint-brush", lib="font-awesome")),
              menuItem("Tables available", tabName="MenuItemTables", icon = icon("table", lib="font-awesome")),
              menuItem("Query tests", tabName="MenuItemQueryTests", icon = icon("table", lib="font-awesome"))
))

## Body ----
# Content of each page (for example the ones listed as menu items, can have tabs or submenus)
# Box colors: https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors

## To avoid a huge file, each page is described in an indepentent file: 
pagesToLoad <- list.files(pattern = "page_*")
pagesToLoad <- grep("^- ", pagesToLoad, invert=T, value=T) # ignore those starting by "- "
invisible(sapply(pagesToLoad, source))

## Indicate where to show each:
body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "HomePage",
            page_home
    ),

    tabItem(tabName = "MenuItemDesign",
            img(src="img/experiment.png", width="60%"),
            page_datasetExpDesign
    ),
    
    tabItem(tabName = "MenuItemAdultCellTypes",
            page_adultCellTypes
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
    
    tabItem(tabName = "MenuItemExamples",
            page_contentExamples
    ),
    
    tabItem(tabName = "MenuItemFigs",
            page_figuresAvailable
    ),
    
    
    tabItem(tabName = "MenuItemTfsCellType",
            page_tfsCellType
    ),
    
    tabItem(tabName = "MenuItemTables",
            page_tablesAvailable
    ),
    
    tabItem(tabName = "MenuItemQueryTests",
            page_queryTests
    )
  )
)

# Other settings
# enableBookmarking(store = "url")

# Finally, build website:
ui <- function(request) { 
  dashboardPage(header, sidebar, body, skin="black")
}

# function(request) {
# fluidPage(
#   # Footer: Do we want one?
#   fluidRow(
#     column(12, align="center",
#            a("Aerts lab", href="https://aertslab.org", target="_blank"), 
#            " @ ",
#            a("VIB", href="https://www.vib.be", target="_blank"),
#            " - ",
#            a("KU Leuven", href="https://www.kuleuven.be", target="_blank")
#   ))
# )
# }