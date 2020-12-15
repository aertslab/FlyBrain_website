require(data.table)
require(ggplot2)
require(plotly)

dotheatmap <- function (enrichmentDf,
                        var.x="Topic", var.y="ID", 
                        var.col="FC", col.low="dodgerblue", col.mid="floralwhite", col.high="brown1", midpoint=0.5,
                        var.size="p.adjust", min.size=1, max.size=8, sizeRange=NULL,
                        ...)
{
  # colorPal <- grDevices::colorRampPalette(c(col.low, col.mid, col.high))
  p <- ggplot(data=enrichmentDf, mapping=aes_string(x=var.x, y=var.y)) +
    geom_point(mapping=aes_string(size=var.size, color=var.col)) +
    scale_radius(range=c(min.size, max.size), limit=sizeRange) +
    # scale_colour_gradientn(colors=colorPal(10)) +
    scale_colour_gradient2(low=col.low, mid=col.mid, high=col.high, midpoint=midpoint) +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1))
  
  return(p)
}
