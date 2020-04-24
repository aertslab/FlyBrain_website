
tableLoadRender <- function(filePath,
                               fileType="rds",
                               columnTooltip=NULL) 
{
    message("loading ", filePath, "...")
    if(fileType=="rds") 
    {
      sst <- data.table(readRDS(filePath))
    }else {
      if(fileType=="rdata") 
      {
        objectName <- load(filePath)
        sst <- eval(as.name(objectName))
        rm(list=objectName)
      }else{
        stop("'fileType' not valid")
      }
    }
    print(dim(sst))
    
    ## For tooltip:
    headerCallback <- NULL
    if(!is.null(columnTooltip))  headerCallback <- JS(paste0(
                                                "function(thead, data, start, end, display){",
                                                "  var tooltips = ", columnTooltip,";",
                                                "  for(var i=0; i<=", ncol(sst),"; i++){",
                                                "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                                "  }",
                                                "}"
                                              ))
    tbl <- DT::renderDataTable(sst, 
                                     filter="top", 
                                     escape=FALSE,
                                     server=TRUE,
                                     extensions=c("ColReorder", "FixedHeader", "Buttons"), # 
                                     options=list(
                                       pageLength = 25
                                       , colReorder=TRUE
                                       , dom = 'riftBpl'
                                       , buttons=c('copy', 'csv', 'pdf')
                                       , scrollX=TRUE
                                       # , scrollY=TRUE # vertical scroll bar within the table
                                       , fixedHeader = TRUE # header visible while scrolling
                                       , headerCallback = headerCallback
                                     )
      )
    return(tbl)
}


## If a module needs to access an input that isn’t part of the module, the containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
#           callModule(myModule, "myModule1", reactive(input$checkbox1))
# input$file or output$file refers to the ns("file")
# Data is only loaded once (I guess it depends on when the module is called...): 
tableLoad.server <- function(input, output, session, # not optional
                                 filePath,
                                 fileType="rds",
                                 columnTooltip=NULL) 
{
  observeEvent(input$load, {
    ### 
    removeUI(
      selector = paste0("#", session$ns(NULL),"-load")
    )
    
    # isolate(print(reactiveValuesToList(input$load)))
    ### Start loading...
    output$tbl <- tableLoadRender(filePath=filePath, fileType=fileType, columnTooltip=columnTooltip) 
  })
}
