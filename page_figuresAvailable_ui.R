page_figuresAvailable <- fluidPage(
  
  tabsetPanel(type = "tabs",
              id = "FiguresAvailable-tabset",
              
              # Tab: 
              tabPanel("3D test",
                       id = "FiguresAvailable-tabDARs",
                       page_tsne3D
              ),
              # Tab: 
              tabPanel("TO DO",
                       id = "FiguresAvailable-tabOther",
                       includeMarkdown("md/figuresAvailable.Rmd")
              )
  )
)
