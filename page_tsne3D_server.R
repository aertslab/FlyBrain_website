library(plotly)

# TODO: Remove, just as example
plot3d.server <- function(input, output, session, dataPath) {
  ## Todo: save coords already with data: load and plot...
  load(paste0(dataPath,"/umap00_3D_larvaPupaCells.RData"))
  drCoords <- drCoords[sample(rownames(drCoords)),]
  colnames(drCoords) <- paste0("dim", seq_len(ncol(drCoords)))
  drCoords <- data.frame(drCoords, varToCol="-", stringsAsFactors = F)
  
  # TODO: How to load a common copy for all functions??
  load(paste0(dataPath,"/cellData_0.2.1.RData")) # TODO 
  load(paste0(dataPath,"/colVars_0.2.1_merged.RData")) # TODO
  
  ######
  # Color by annot
  # varName <- "annotation_v02"
  # colVars[[varName]]["-"] <- "#fafafa01"
  # cells <- intersect(rownames(cellData)[which(!is.na(cellData[,"annotation_v02.1"]))], rownames(drCoords))
  # drCoords[cells,varName] <- as.character(cellData[cells,varName])

  # # Stage
  varName <- "stage"
  colVars[[varName]]["-"] <- "#fafafa01"
  cells <- intersect(rownames(cellData)[which(!is.na(cellData[,varName]))], rownames(drCoords))
  drCoords[cells,varName] <- as.character(cellData[cells,varName])
  
  # TODO: Have the drCoords object ready... like this it is a bit slow...
  # Color by clusters
  load(paste0(dataPath,"/upto12h_900fip__05k_5eps_55clusters.RData"))
  cells <- intersect(rownames(cellData), names(clusters))
  cellData$clusters <- "-"
  cellData[cells,"clusters"] <- clusters[cells]
  source('libs/col_vector.R')
  col_vector <- c(col_vector[-1],col_vector[-1])
  colVars[["clusters"]] <- setNames(col_vector, seq_along(col_vector))
  varName <- "clusters"
  colVars[[varName]]["-"] <- "#fafafa01"
  cells <- intersect(rownames(drCoords),rownames(cellData)[which(!is.na(cellData[,varName]))])
  drCoords[cells,varName] <- as.character(cellData[cells,varName])
  ######
  
  drCoords$cell <- rownames(drCoords)
  rownames(drCoords) <- NULL
  
  observe({
    
    print(input$plot3d_pntCol)
    drCoords$varToCol <- drCoords[,input$plot3d_pntCol]
    colVar <- colVars[[input$plot3d_pntCol]]
      
    print(input$plot3d_perCells)
    nCells <- ceiling(nrow(drCoords)*(input$plot3d_perCells/100))
    
    plotType <- "scatter"
    if("dim3" %in% colnames(drCoords)) plotType="scatter3d"
    
    # setup default settings: zoom...
    p <- plot_ly(drCoords[seq_len(nCells),], x = ~dim1, y = ~dim2, z = ~dim3, type=plotType, mode="markers",
                 color = ~varToCol, colors=colVar, marker=list(size = input$plot3d_pntSize),
                 hoverinfo = 'text', text= ~paste("</br>Cell: ", cell, "</br>Value: ", varToCol))
    output$plot_3d <- renderPlotly(p)
    
    # rbokeh::figure(logo=NULL) %>%
    #                     color = getAUC(auc)[input$geneSetBokeh,rownames(tSNE.df)], legend=FALSE, lname = "cells") %>%
    #   rbokeh::set_palette(continuous_color = rbokeh::pal_gradient(c("lightgrey", "pink", "red"))) %>%
    #   rbokeh::tool_lasso_select(callback = rbokeh::shiny_callback(id="cellsSelected"), "cells")
  })
}