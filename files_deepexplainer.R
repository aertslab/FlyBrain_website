setwd("/ddn1/vol1/staging/leuven/stg_00002/lcb/saibar/Projects/FB_devel/20200420_Website/FlyBrain_website/www/deepExplainer/img")
sourceDir <- "/staging/leuven/stg_00002/lcb/itask/files/deepFLY/figures/katina_GRN_deepexplainer/"
pngFiles <- list.files(sourceDir,include.dirs = T, recursive = T, pattern="png"); length(pngFiles)
# pdfFiles <- list.files(sourceDir,include.dirs = T, recursive = T, pattern="pdf")

# Copy: 
sapply(pngFiles, function(x) file.copy(file.path(sourceDir, x), gsub("/", "___",x, fixed=T)))
sapply(list.files(), function(x) file.rename(x, gsub(":", "-", x)))


# Create thumbnails:
library("EBImage")

# Calculate area to crop:
# tmp <- rowSums(imageData(x))# apply(, 1, min)
# which(tmp < max(tmp))[1]
# tail(which(tmp < max(tmp)))
pxStart <- 2700
pxEnd <- 21700
# plot(tmp, cex=.5, pch=16); abline(v=c(pxStart, pxEnd))

mainFiles <- list.files(pattern = ".png")
mainFiles <- grep("_th", mainFiles, invert=T, value=T)
for(imgFile in mainFiles)
{
  print(imgFile)
  # Create thumbnail
  thFileName <- gsub(".png","_th.png", imgFile)
  if(!file.exists(thFileName))
  {
    x <- readImage(imgFile) # ; display(x)
    x <- x[pxStart:pxEnd,,] # crop
    
    writeImage(x, imgFile)  # save cropped full size
    
    x <- resize(x, dim(x)[1]/4, dim(x)[2]/3)
    writeImage(x, thFileName)  
  }
} 




