library(shiny)
library(plotly) 

### ui ----
plot_EnhancerDL.ui <- function(id){
  ns <- NS(id)
  
  fluidPage( 
    fluidRow(
      selectInput(inputId=NS(id, "enhID"),
                label = "Select enhancer:",
                choices = NULL,
                selectize = TRUE)),
    fluidRow(
    column(10, 
      # imageOutput(NS(id, "dlPlot"), height="100px", inline=T))# "10%")  # Does not resize... -.-
      uiOutput(NS(id, "deepExplainerPlot")),
      uiOutput(NS(id, "fullSizeLink"))
    )) # %>% withSpinner(color="#0dc5c1")
  )
}

### server ----
plot_EnhancerDL.server <- function(input, output, session, imgRootPath) { 
 
  # Load: 
  enhFiles <- list.files(file.path(imgRootPath, 'deepExplainer/thumbnails'), pattern=".png")
  enhFiles <- unique(gsub("_th.png", "", enhFiles))
  
  enhFiles <- data.frame(fileName=enhFiles, stringsAsFactors=F)
  enhFiles$cellType <- sapply(strsplit(enhFiles$fileName, "___"), function(x) x[[1]])
  enhFiles$displayName <- sapply(strsplit(enhFiles$fileName, "___"), function(x) paste0(x[[2]], " (", x[[1]], ")"))
  enhFiles$displayName <- sapply(strsplit(enhFiles$displayName, "__"), function(x) x[[2]])
  rownames(enhFiles) <- enhFiles$displayName
  
  updateSelectInput(session, "enhID", choices=enhFiles$displayName, selected=enhFiles$displayName)
  
  # Start traking events (plots to show):
  observe({
    enhSelected <- input$enhID 
    print(enhSelected)
    
    if(!is.na(enhSelected)){
      # output$dlPlot <- renderImage(list(src = imgPath,contentType = "image/png", alt="Enhancer view"), delete=FALSE) 
      output$deepExplainerPlot <- renderUI({
        tagList(img(src=file.path(file.path('deepExplainer/thumbnails', paste(enhFiles[enhSelected, "fileName"], '_th.png', sep=''))), width="100%"))
      })
      
      output$fullSizeLink <- renderUI({
        tagList(a("[View full size]", 
                  href=file.path(file.path('deepExplainer', enhFiles[enhSelected, "cellType"], paste(enhFiles[enhSelected, "fileName"], '.png', sep=''))), 
                  target="_blank"))
      })
    }
  })
}

# Build page ----
page_enhancerArchitecture <- plot_EnhancerDL.ui("plot_DL")
