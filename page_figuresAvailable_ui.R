source('page_tfsCellType_ui.R')
page_figuresAvailable <- fluidPage(
  includeMarkdown("md/figuresAvailable.Rmd"),
  
  tabsetPanel(type = "tabs",
              id = "FiguresAvailable-tabset",
              
              # Tab: 
              tabPanel("3D test",
                       id = "FiguresAvailable-tabDARs",
                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
                       page_tsne3D
              ),
              
              # Tab: 
              tabPanel("dotplot",
                       id = "FiguresAvailable-tabTfsCellType",
                       # includeMarkdown("md/tmp_dataTablesDescr/tbl_DAR.Rmd"),
                       page_tfsCellType
              )
  )
)
