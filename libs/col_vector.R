
library(RColorBrewer)
modColProd<- function(acolor, factor=.5) rgb(t(as.matrix(apply(abs(col2rgb(acolor)*factor), 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
modColSum<- function(acolor, factor=120) rgb(t(as.matrix(apply(abs(col2rgb(acolor)+factor), 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
paletteNames <- c("Set1", "Accent", "Dark2", "Paired"); col_vector <- suppressWarnings(unlist(mapply(brewer.pal, brewer.pal.info[paletteNames,]$maxcolors, paletteNames)))
col_vector <- c(col_vector, rainbow(15), sapply(rainbow(5), modColProd),sapply(rainbow(5), modColSum))
col_vector <- col_vector[-c(9,17,25,31:33,36)]
set.seed(123); col_vector <- c("#30303030", sample(col_vector))
# pie(rep(1,length(col_vector)),col=col_vector)
# length(col_vector)
