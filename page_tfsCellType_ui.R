library(shiny)
library(plotly)
### Prepare components ----
dotPlot.ui <- function(id){
  ns <- NS(id)
  fluidPage(
        plotlyOutput(NS(id, "plot_tfsCellType"), height="900px") %>% withSpinner(color="#0dc5c1")
    )
}

### Build page ----
page_tfsCellType <- dotPlot.ui("plot_dotplot") 




