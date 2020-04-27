page_tfsCellType <- fluidPage(
  
  tabsetPanel(type = "tabs",
              id = "TfsCellType-tabset",
              
              # Tab: 
              tabPanel("Dotplot",
                       id = "TfsCellType-dotplot",
                       page_tfsCellType_dotplot
              ),
              # Tab: 
              tabPanel("Tfs details",
                       id = "TfsCellType-tfDetails",
                       "TO DO"
                       # includeMarkdown("md/figuresAvailable.Rmd")
              )
  )
)
