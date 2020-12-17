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
    ))
  )
}

### server ----
plot_EnhancerDL.server <- function(input, output, session, imgRootPath) { 
 
  # Load: 
  enhFiles <- list.files(file.path(imgRootPath, 'deepExplainer/thumbnails'), pattern=".png")
  enhFiles <- unique(gsub("_th.png", "", enhFiles))
  names(enhFiles) <- enhFiles
  enhFiles <- setNames(enhFiles, sapply(strsplit(enhFiles, "___"), function(x) paste0(x[[2]], " (", x[[1]], ")")))
  names(enhFiles) <- sapply(strsplit(names(enhFiles), "__"), function(x) x[[2]])
  
  updateSelectInput(session, "enhID", choices=names(enhFiles), selected=names(enhFiles)[1])
  
  # Start traking events (plots to show):
  observe({
    enhSelected <- enhFiles[input$enhID]
    print(enhSelected)
    
    if(!is.na(enhSelected)){
      # output$dlPlot <- renderImage(list(src = imgPath,contentType = "image/png", alt="Enhancer view"), delete=FALSE) 
      output$deepExplainerPlot <- renderUI({
        tagList(img(src=normalizePath(file.path('deepExplainer/thumbnails', paste(enhSelected, '_th.png', sep=''))), width="100%"))
      })
      
      output$fullSizeLink <- renderUI({
        tagList(a("[View full size]", 
                  href=normalizePath(file.path('deepExplainer', paste(enhSelected, '.png', sep=''))), 
                  target="_blank"))
      })
    }
  })
}

# Build page ----
page_enhancerArchitecture <- plot_EnhancerDL.ui("plot_DL")
