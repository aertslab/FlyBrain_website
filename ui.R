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
              menuItem("Home", tabName = "IntroPage", icon = icon("home", lib="font-awesome")), 
              menuItem("The datasets", tabName = "nothing", icon = icon("seedling", lib="font-awesome"),
                       menuSubItem("Overview", tabName = "main"),
                       menuSubItem("Adult brain scATAC", tabName = "MenuItem1-sub1"),
                       menuSubItem("Larval brain scATAC", tabName = "MenuItem1-sub4"),
                       menuSubItem("scATAC across development", tabName = "MenuItem1-sub3")),
              menuItem("Adult fly brain cell types", tabName = "MenuItem2", icon = icon("brain", lib="font-awesome"), # "disease" icon does not work?   / certificate
                       menuItem("Neuronal", tabName = "MenuItem2-sub1", menuSubItem("Central brain", tabName = "MenuItem2-sub1-sub1"), menuSubItem("Mushroom Body", tabName = "MenuItem2-sub1-sub1"), menuSubItem("Optic Lobe", tabName = "MenuItem2-sub1-sub2")),
                       menuItem("Glial", tabName = "MenuItem2-sub2")),
              menuItem("Cell types across development", tabName = "MenuItem6", icon = icon("code-branch", lib="font-awesome")), # egg does not work
              menuItem("Experimental design", tabName = "MenuItem5", icon = icon("flask", lib="font-awesome")),
              menuItem("Stats", tabName = "MenuItemStats1", icon = icon("bar-chart", lib="font-awesome")),
              menuItem("FAQ", tabName = "MenuItemStats", icon = icon("question-circle", lib="font-awesome")),
              menuItem("Data Downloads", tabName = "MenuItemResources", icon = icon("cloud-download-alt", lib="font-awesome")),
              menuItem("About us", tabName = "MenuItemAbout", icon = icon("user", lib="font-awesome")),
              hr(),
              
              menuItem("Content examples", tabName = "MenuItemExamples", icon = icon("play", lib="font-awesome")),
              menuItem("Figures available", tabName = "MenuItemFigs", icon = icon("paint-brush", lib="font-awesome")),
              menuItem("Tables available", tabName = "MenuItemTables", icon = icon("table", lib="font-awesome"))
  )
)


## Body ----
# Content of each page (for example the ones listed as menu items, can have tabs or submenus)
# Box colors: https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors

## To avoid a huge file, each page is described in an indepentent file: 
invisible(sapply(list.files(pattern = "page_*"), source))
# source("page_introToDataset_ui.R")
# source("page_contentExamples_ui.R")
# source("page_tablesAvailable_ui.R")

## Indicate where to show each:
body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "IntroPage",
            fluidPage(
              h2("Homepage"))
    ),
    
    tabItem(tabName = "main",
            page_introToDataset
    ),
    
    tabItem(tabName = "MenuItem1-sub1",
                    fluidPage(
                      h2("adult brain"))
    ),

    tabItem(tabName = "MenuItem2"
            # h2("If it has sub-items, this will not be shown...")
    ),

    tabItem(tabName = "MenuItem2-sub2",
            # Move to external file:
            fluidPage(
              h2("sub-item 2 content....")
            )
    ),
    
    tabItem(tabName = "MenuItem3",
            # Move to external file:
            fluidPage(
              h2("this page's title")
            )
    ),
    
    tabItem(tabName = "MenuItem4",
            # Move to external file:
            fluidPage(
              h2("page 4 title")
            )
    ),
    
    tabItem(tabName = "MenuItem5",
            page_resources
    ),
    
    tabItem(tabName = "MenuItemStats",
            page_faq
    ),
    
    tabItem(tabName = "MenuItemResources",
            # Move to external file:
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
  dashboardPage(header, sidebar, body, skin = "black")
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