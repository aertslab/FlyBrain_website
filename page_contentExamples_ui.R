source("page_tsne3D_ui.R") # why needed??

### Prepare components ----
histPlot.ui <- function(id) {
  ns <- NS(id) # The first line: creates a namespace with the "id" (always the first argument of the function)
  fluidRow(
    box(
      title = "Controls",
      collapsible = TRUE,
      # background="aqua",
      status="warning", solidHeader=TRUE,
      "Box content here", br(), "More box content",
      sliderInput(NS(id, "slider"), "Number of observations:", 0, 500, 50)
    ), 
    box(
      solidHeader = TRUE,
      collapsible = TRUE,
      # background = "maroon",
      plotOutput(NS(id, "plot1")) # , height = 250
    )
  )
}

sampleTable.ui <- function(id){
        DT::datatable(iris,
                      filter="top",
                      extensions=c('ColReorder','Buttons' , "FixedHeader"),
                      options=list(
                        pageLength = 100
                        , colReorder=TRUE
                        , dom = 'Bfrtip'
                        , buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
                        , fixedHeader = TRUE # header visible while scrolling
                      )
        )
}

sampleColumnLayout.ui <- function(id){
  fluidPage(
    sidebarPanel(
      "some controls here..."
    ),
    mainPanel(
      fluidRow(
        "a figure or table here",
        )
      )
    )
}

library(visNetwork)
sampleNetwork.ui <- function(id){
  fluidPage(
    sidebarPanel(
      "some controls here..."
    ),
    mainPanel(
      fluidRow(
        visNetworkOutput(NS(id, "plot_nwExample"), height="900px"),
      )
    )
  )
}

### Build page ----
page_contentExamples <- fluidPage(
  
  h2("random content example:"),

  tabsetPanel(type = "tabs",
    id = "MenuItem1-tabset",
    # title = "This tabBox title",
    # height = "500px",

    #  ~ MenuItem1-tab2 ----
    tabPanel("Network",
             id = "MenuItem8-tab2",
             sampleNetwork.ui("tab_networkExample")
    ),
    
    # ~ MenuItem1-tab1 ----
    tabPanel("Interactive plot",
             id = "MenuItem8-tab1", # Ids need to be unique!
             fluidPage(
               fluidRow( # Each row...
                 h3("Some content here"),
                 br()
                 # text("More  content")
               ),
               histPlot.ui("tab_plotOne")
             )
    ),

    #  ~ MenuItem1-tab3 ----
    tabPanel("2-column layout",
             id = "MenuItem8-tab3",
             sampleColumnLayout.ui()
    )
  )
)
