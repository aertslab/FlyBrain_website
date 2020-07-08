# 
plotContinuous <- function(
  drCoords,
  variable,
  colorPals=list("high"=grDevices::colorRampPalette(c("red", "darkred"))(10),
                "low"=grDevices::colorRampPalette(c("skyblue", "#f0f0f0"))(10)),
  palBreaks=0,
  minMaxVal=.20,
  showLegend=TRUE,
  alphaDefault=.8, # for higher/default palette (first)
  alphaOff=.2, # all other palettes
  ...
){
  if(!is.null(palBreaks)) if(length(colorPals) != (length(palBreaks)+1)) stop("number of palettes should be number of breaks+1")
  # variable <- variable[,rownames(drCoords),drop=FALSE] # slower if repeated many times...
  if(!all(names(variable) == rownames(drCoords))) stop("colnames of variable and coords do not match")
  cellColor <- setNames(rep(NA, length(variable)), names(variable))
  
  ### Higher palette (e.g. positive values)
  if(length(colorPals)>1) highPal <- colorPals[[1]]
  highPal <- adjustcolor(highPal, alpha=alphaDefault)
  if(!is.null(alphaOff)) highPal[1] <- adjustcolor(highPal[1], alpha=alphaOff)
  
  tmpVar <- variable
  if(!is.null(palBreaks[1])) tmpVar <- variable[which(variable>=palBreaks[1])]
  cellColor[names(tmpVar)] <- highPal[as.numeric(cut(tmpVar
                                     , breaks=seq(min(c(palBreaks[1],tmpVar)), max(c(minMaxVal,tmpVar)), length.out=length(highPal))
                                     , right=F,include.lowest=T))]
  
  
  ### Next palettes
  palBreaks <- rev(sort(palBreaks))
  for(i in seq_along(palBreaks))
  {
    colPal <- colorPals[[i+1]]
    if(!is.null(alphaOff)) colPal <- adjustcolor(colPal, alpha=alphaOff)
    
    tmpVar <- variable[which(variable < palBreaks[i])]
    tmpVar <- tmpVar[which(tmpVar >= c(palBreaks, -Inf)[i+1])]
    if(length(tmpVar)>0)
    {
      cellColor[names(tmpVar)] <- colPal[as.numeric(cut(tmpVar
                                                        , breaks=seq(min(c(tmpVar)), max(c(palBreaks,tmpVar)), length.out=length(colPal))
                                                        , right=F,include.lowest=T))] 
    } 
  }
  
  # Plot           
  plot(drCoords,  pch=16, 
      , col=cellColor
      , ylab=""
      , xlab=paste("max:", signif(max(variable),2))
      , axes=F
      , ...)
  box(col="lightgrey")
     
  # plot.new()
  if(showLegend) .vertical.image.legend(c(min(c(0,variable)), max(c(minMaxVal,variable))), colPal)
}

### Aux. Copied from cisTopic
.vertical.image.legend <- function(zlim, col){
  starting.par.settings <- par(no.readonly=TRUE)
  mai <- par("mai")
  fin <- par("fin")
  x.legend.fig <- c( 1.0-(mai[4]/fin[1]), 1.0 )
  y.legend.fig <- c( mai[1]/fin[2], 1.0-(mai[3]/fin[2]) )
  x.legend.plt <- c( x.legend.fig[1]+(0.08*(x.legend.fig[2]-x.legend.fig[1])),
                     x.legend.fig[2]-(0.6*(x.legend.fig[2]-x.legend.fig[1])) )
  y.legend.plt <- y.legend.fig
  cut.pts <- seq(zlim[1], zlim[2], length=length(col)+1)
  z <- ( cut.pts[1:length(col)] + cut.pts[2:(length(col)+1)] ) / 2
  par(new=TRUE, pty="m", plt=c(x.legend.plt, y.legend.plt), bty='o')
  image(x=1, y=z, z=matrix(z, nrow=1, ncol=length(col)),
        col=col, xlab="", ylab="", xaxt="n", yaxt="n")
  axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis=0.6, tcl=-0.1)
  box()
  mfg.settings <- par()$mfg
  par(starting.par.settings)
  par(mfg=mfg.settings, new=FALSE)
}