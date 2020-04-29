
page_tfsCellType <- fluidPage(
  
  tabsetPanel(type = "tabs",
              id = "TfsCellType-tabset",
              
              # Tab: 
              tabPanel("Dotplot (high conf annot)",
                       id = "TfsCellType-dotplot",
                       page_tfsCellType_dotplot
              ),
              
              # Tab: 
              tabPanel("Dotplot (any annot)",
                       id = "TfsCellType-dotplot2",
                       "TFs appear twice: Those without sufix are the same as in the other tab ('high conf': direc & ort), the ones with '[LC]' sufix also use 'motif similarity' for Motif-TF annotation.",
                       br(),
                       "(If keeping both plots, move to same tab with a 'select')",
                       page_tfsCellType_dotplot2,
                       br(),
                       "Note: T4/T5 are not distinguishable in the scRNA-seq dataset, so the same RNA-cluster has been taken as reference for both cell types"
              ),
              
              # Tab: 
              tabPanel("Tfs details",
                       id = "TfsCellType-tfDetails",
                       "TO DO"
                       # includeMarkdown("md/figuresAvailable.Rmd")
              )
  )
)

