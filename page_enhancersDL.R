library(shiny)
library(plotly) 
library(dplyr)
library(GenomicRanges)

### ui ----
plot_EnhancerDL.ui <- function(id){
  ns <- NS(id)
  
  fluidPage( 
    fluidRow(
      selectInput(inputId=NS(id, "cellType"),
                label = "Cell type:",
                choices = NULL,
                selectize = TRUE),
      selectInput(inputId=NS(id, "enhID"),
                label = "Enhancer:",
                choices = NULL,
                selectize = TRUE)),
    fluidRow(
    column(10, 
      # imageOutput(NS(id, "dlPlot"), height="100px", inline=T))# "10%")  # Does not resize... -.-
      uiOutput(NS(id, "regionTitle")),
      uiOutput(NS(id, "deepExplainerPlot")),
      uiOutput(NS(id, "fullSizeLink")),
      uiOutput(NS(id, "ucscLink")),
    )) # %>% withSpinner(color="#0dc5c1")
  )
}

### server ----
plot_EnhancerDL.server <- function(input, output, session, imgRootPath, dataPath) { 
  
  enhFiles <- readRDS(paste0(dataPath, file="DL_enhFiles.Rds"))
  cellTypes <- unique(enhFiles$cellType)
  
  # Initialization: 
  celltypeSelected <- cellTypes[1]
  updateSelectInput(session, "cellType", choices=cellTypes, selected=celltypeSelected)
  ctEnhancers <- enhFiles %>% filter(cellType==celltypeSelected) %>% dplyr::select(enhancer) 
  ctEnhancers <- as.character(ctEnhancers[,1])
  updateSelectInput(session, "enhID", choices=ctEnhancers, selected=ctEnhancers[2])

  # Start traking events (plots to show):
  observe({
    celltypeSelected <- input$cellType
    # print(celltypeSelected)
    
    if(!is.na(celltypeSelected)){
      ctEnhancers <- enhFiles %>% filter(cellType==celltypeSelected) %>% dplyr::select(enhancer)
      ctEnhancers <- as.character(ctEnhancers[,1])
      updateSelectInput(session, "enhID", choices=ctEnhancers, selected=ctEnhancers[1])
    }
  })
  
  observe({
    enhSelected <- input$enhID
    celltypeSelected <- input$cellType
    # print(enhSelected)
    
    if((!is.na(enhSelected)) && (enhSelected!="")){
      # output$dlPlot <- renderImage(list(src = imgPath,contentType = "image/png", alt="Enhancer view"), delete=FALSE) 
       
      enhFileName <- enhFiles %>% filter((cellType==celltypeSelected) & (enhancer==enhSelected)) %>% dplyr::select(fileName) %>% as.character()
      
      thumbFile <- file.path('deepExplainer/thumbnails', paste0(enhFileName, '_th.png'))
      output$deepExplainerPlot <- renderUI(NULL)
      
        if(file.exists(file.path(imgRootPath, thumbFile))){
          output$deepExplainerPlot <- renderUI({tagList(img(src=thumbFile, alt='enhancer preview', width="100%"))})
        }
        else{
          output$deepExplainerPlot <- renderUI({tagList(' - Thumbnail not available yet. Download the full file. -')})
        }
      
      output$fullSizeLink <- renderUI({
        tagList(a("[View full size image]", 
                  href=file.path(file.path('deepExplainer', celltypeSelected, paste0(enhFileName, '.png'))), 
                  target="_blank"))
      })
      
      output$regionTitle <- renderUI({tagList(enhFileName)}) # , "(", celltypeSelected, " enhancer: ", enhSelected, " region scored: ", "", ")"
      
      toShow <- as.character(resize(GRanges(sub("-", ":", enhSelected)), width=5000, fix="center"))
      toShow <- sub(":", "%3A", toShow); toShow <- sub("-", "%2D", toShow)
      enhSelected <- sub("-", "%3A", enhSelected); enhSelected <- sub("-", "%2D", enhSelected)
      # toShow <- enhSelected
      ucscURL <- paste0("http://genome.ucsc.edu/cgi-bin/hgTracks?db=dm6&hubUrl=http://ucsctracks.aertslab.org/papers/FlyBrain/hub.txt&position=", toShow,
                        "&highlight=dm6.", enhSelected, "%AAAAAACC")
      
      output$ucscLink <- renderUI(
        tagList(a("[View region in UCSC Genome Browser]", 
                  href=ucscURL, 
                  target="_blank")))
    }
  })
}

# Build page ----
page_enhancerArchitecture <- plot_EnhancerDL.ui("plot_DL")
