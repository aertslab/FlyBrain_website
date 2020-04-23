# shiny::runApp('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/...')
for(pkg in c("plotly","visNetwork","DT", "shinydashboard")) if(!pkg %in% rownames(installed.packages())) install.packages(pkg)

library(shiny)
library(shinydashboard)
library(DT)

## Header  ----
header <- dashboardHeader(title = "scATAC of the Fly brain development",
                          tags$li(class = "dropdown", bookmarkButton(label="Link to this view")),
                          disable=FALSE)

## Sidebar  ----
# List of links to include in the sidebar
# https://rstudio.github.io/shinydashboard/behavior.html
sidebar <- dashboardSidebar(
  sidebarMenu(id="pages",
    # https://fontawesome.com/icons ; https://icons.getbootstrap.com/ 
    menuItem("Dataset", tabName = "IntoPage", icon = icon("brain", lib="font-awesome")),
    menuItem("Cell types", tabName = "MenuItem2", icon = icon("certificate", lib="font-awesome"), # "disease" icon does not work?   / certificate
             menuSubItem("Adult", tabName = "MenuItem2-sub1"),
             menuSubItem("Larva / development", tabName = "MenuItem2-sub2")
             ),
    menuItem("Networks", tabName = "MenuItem3", icon = icon("project-diagram", lib="font-awesome")),
    menuItem("DL", tabName = "MenuItem4", icon = icon("chart-area", lib="font-awesome")),
    menuItem("Development", tabName = "MenuItem5", icon = icon("bullseye", lib="font-awesome")), # egg does not work
    menuItem("Stats & FAQ", tabName = "MenuItemStats", icon = icon("bar-chart", lib="font-awesome")),
    menuItem("Data list & Downloads", tabName = "MenuItemResources", icon = icon("cloud-download-alt", lib="font-awesome")),
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
    tabItem(tabName = "IntoPage",
            page_introToDataset 
    ),
  
    tabItem(tabName = "MenuItem2"
            # h2("If it has sub-items, this will not be shown...")
    ),
    tabItem(tabName = "MenuItem2-sub1",
            # Move to external file:
            fluidPage(
            h2("sub-item 1 content....")
            )
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
    )
  )
)

# Other settings
enableBookmarking(store = "url")

# Finally, build website:
fluidPage(
    dashboardPage(header, sidebar, body, skin = "black"), # green
    # Footer: Do we want one?
    fluidRow(
      column(12, align="center",
             a("Aerts lab", href="https://aertslab.org", target="_blank"), 
             " @ ",
             a("VIB", href="https://www.vib.be", target="_blank"),
             " - ",
             a("KU Leuven", href="https://www.kuleuven.be", target="_blank")
    ))
)
