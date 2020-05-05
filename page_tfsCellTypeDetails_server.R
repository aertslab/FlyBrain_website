library(plotly)

plot_acc_tsne.server <- function(input, output, session, dataPath) {
  #meanAccMat <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/meanAccMat_forTsne.Rds")
  drCoords <- readRDS("/ddn1/vol1/staging/leuven/stg_00002/lcb/dpapasok/tfsPerCellType/drCoords_adultTsne.Rds")
  colnames(drCoords) <- paste0("dim", seq_len(ncol(drCoords)))
  drCoords <- data.frame(drCoords, varToCol="-", stringsAsFactors = F)
  
  tf <- "ey"
  #source('/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/aux_scripts/cisTopic/plotCont.R')
  #cellVar <- setNames(rep(0,nrow(drCoords)), rownames(drCoords))
  #cellVar[colnames(meanAccMat)] <- meanAccMat[tf, colnames(meanAccMat)]*10**6
  #head(cellVar)
  # plotContinuous(drCoords, cellVar,
  #                colorPals=list("high"=grDevices::colorRampPalette(c("mistyrose", "red"))(10),
  #                               "low"=grDevices::colorRampPalette(c("mistyrose", "red"))(10)),
  #                palBreaks=0, cex=0.6)
  
  
  observe({
    
  #   
  #   print(input$plot3d_pntCol)
  #   drCoords$varToCol <- drCoords[,input$plot3d_pntCol]
  #   colVar <- colVars[[input$plot3d_pntCol]]
  #   
  #   print(input$plot3d_perCells)
  #   nCells <- ceiling(nrow(drCoords)*(input$plot3d_perCells/100))
  #   
  #   plotType <- "scatter"
  #   if("dim3" %in% colnames(drCoords)) plotType="scatter3d"
  #   
  #   # setup default settings: zoom...
  #   p <- plot_ly(drCoords[seq_len(nCells),], x = ~dim1, y = ~dim2, z = ~dim3, type=plotType, mode="markers",
  #                color = ~varToCol, colors=colVar, marker=list(size = input$plot3d_pntSize),
  #                hoverinfo = 'text', text= ~paste("</br>Cell: ", cell, "</br>Value: ", varToCol))
  #   output$accessibility_tsne_plot" <- renderPlotly(p)
  #   
  #   # rbokeh::figure(logo=NULL) %>%
  #   #                     color = getAUC(auc)[input$geneSetBokeh,rownames(tSNE.df)], legend=FALSE, lname = "cells") %>%
  #   #   rbokeh::set_palette(continuous_color = rbokeh::pal_gradient(c("lightgrey", "pink", "red"))) %>%
  #   #   rbokeh::tool_lasso_select(callback = rbokeh::shiny_callback(id="cellsSelected"), "cells")
      p <- plot_ly(drCoords, x = ~dim1, y = ~dim2, type="scatter", mode="markers")
      #             color = ~varToCol, colors=colVar, marker=list(size = input$plot3d_pntSize),
      #             hoverinfo = 'text', text= ~paste("</br>Cell: ", cell, "</br>Value: ", varToCol))
      output$accessibility_tsne_plot <- renderPlotly(p)

    
  })
}

# ## plotContinuous function
# 
# plotContinuous <- function(
#   drCoords,
#   variable,
#   colorPals=list("high"=grDevices::colorRampPalette(c("red", "darkred"))(10),
#                  "low"=grDevices::colorRampPalette(c("skyblue", "#f0f0f0"))(10)),
#   palBreaks=0,
#   minMaxVal=.20,
#   showLegend=TRUE,
#   alphaDefault=.8, # for higher/default palette (first)
#   alphaOff=.2, # all other palettes
#   ...
# ){
#   if(!is.null(palBreaks)) if(length(colorPals) != (length(palBreaks)+1)) stop("number of palettes should be number of breaks+1")
#   # variable <- variable[,rownames(drCoords),drop=FALSE] # slower if repeated many times...
#   if(!all(names(variable) == rownames(drCoords))) stop("colnames of variable and coords do not match")
#   cellColor <- setNames(rep(NA, length(variable)), names(variable))
#   
#   ### Higher palette (e.g. positive values)
#   if(length(colorPals)>1) highPal <- colorPals[[1]]
#   highPal <- adjustcolor(highPal, alpha=alphaDefault)
#   if(!is.null(alphaOff)) highPal[1] <- adjustcolor(highPal[1], alpha=alphaOff)
#   
#   tmpVar <- variable
#   if(!is.null(palBreaks[1])) tmpVar <- variable[which(variable>=palBreaks[1])]
#   cellColor[names(tmpVar)] <- highPal[as.numeric(cut(tmpVar
#                                                      , breaks=seq(min(c(palBreaks[1],tmpVar)), max(c(minMaxVal,tmpVar)), length.out=length(highPal))
#                                                      , right=F,include.lowest=T))]
#   
#   
#   ### Next palettes
#   palBreaks <- rev(sort(palBreaks))
#   for(i in seq_along(palBreaks))
#   {
#     colPal <- colorPals[[i+1]]
#     if(!is.null(alphaOff)) colPal <- adjustcolor(colPal, alpha=alphaOff)
#     
#     tmpVar <- variable[which(variable < palBreaks[i])]
#     tmpVar <- tmpVar[which(tmpVar >= c(palBreaks, -Inf)[i+1])]
#     if(length(tmpVar)>0)
#     {
#       cellColor[names(tmpVar)] <- colPal[as.numeric(cut(tmpVar
#                                                         , breaks=seq(min(c(tmpVar)), max(c(palBreaks,tmpVar)), length.out=length(colPal))
#                                                         , right=F,include.lowest=T))] 
#     } 
#   }
#   
#   # Plot           
#   plot(drCoords,  pch=16, 
#        , col=cellColor
#        , ylab=""
#        , xlab=paste("max:", signif(max(variable),2))
#        , axes=F
#        , ...)
#   box(col="lightgrey")
#   
#   # plot.new()
#   if(showLegend) .vertical.image.legend(c(min(c(0,variable)), max(c(minMaxVal,variable))), colPal)
# }
# 
# ### Aux. Copied from cisTopic
# .vertical.image.legend <- function(zlim, col){
#   starting.par.settings <- par(no.readonly=TRUE)
#   mai <- par("mai")
#   fin <- par("fin")
#   x.legend.fig <- c( 1.0-(mai[4]/fin[1]), 1.0 )
#   y.legend.fig <- c( mai[1]/fin[2], 1.0-(mai[3]/fin[2]) )
#   x.legend.plt <- c( x.legend.fig[1]+(0.08*(x.legend.fig[2]-x.legend.fig[1])),
#                      x.legend.fig[2]-(0.6*(x.legend.fig[2]-x.legend.fig[1])) )
#   y.legend.plt <- y.legend.fig
#   cut.pts <- seq(zlim[1], zlim[2], length=length(col)+1)
#   z <- ( cut.pts[1:length(col)] + cut.pts[2:(length(col)+1)] ) / 2
#   par(new=TRUE, pty="m", plt=c(x.legend.plt, y.legend.plt), bty='o')
#   image(x=1, y=z, z=matrix(z, nrow=1, ncol=length(col)),
#         col=col, xlab="", ylab="", xaxt="n", yaxt="n")
#   axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis=0.6, tcl=-0.1)
#   box()
#   mfg.settings <- par()$mfg
#   par(starting.par.settings)
#   par(mfg=mfg.settings, new=FALSE)
# }