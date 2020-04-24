source('page_tfsCellType_ui.R')
page_figuresAvailable <- fluidPage(
  # includeMarkdown("md/figuresAvailable.Rmd"),
  
  tabsetPanel(type = "tabs",
              id = "FiguresAvailable-tabset",
              
              # Tab: 
              tabPanel("TO DO",
                       id = "FiguresAvailable-tabOther",
                       includeMarkdown("md/figuresAvailable.Rmd")
              ), 
              
              # Tab: 
              tabPanel("3D test",
                       id = "FiguresAvailable-tabDARs",
                       page_tsne3D
              ),
              
              # Tab: 
              tabPanel("dotplot",
                       id = "FiguresAvailable-tabTfsCellType",
                       page_tfsCellType
              )
  )
)
