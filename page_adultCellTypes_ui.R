page_adultCellTypes<- fluidPage(
  
  tabsetPanel(type = "tabs",
  id = "celltypes-tabset",
  
  # Tab: 
  tabPanel("Cell type annotation on scATAC data",
           id = "cell-types-descr",
           includeMarkdown("md/adultCellTypes_overview.Rmd"),
           h4("scATAC tSNE"),
           fluidRow(
             img(src="img/tsne.png", width="50%")
           )
           
  ),
  
  tabPanel("Central brain cell types",
           id = "cell-types-cb",
           includeMarkdown("md/adultCellTypes_neuronal.Rmd")
  ),
  
  tabPanel("Optic lobe cell types",
           id = "cell-types-ol"
  ),
  
  tabPanel("Glial cell types",
           id = "cell-types-glia"
  ),
  
  tabPanel("DARs",
           id = "dars"
  )
  
  ))

