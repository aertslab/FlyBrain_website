# UI ----
tableLoad.ui <- function(id){ 
  ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
  
  fluidPage(
    DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
  )
}

# Server ----
formatColumnFilters <- function(tableToShow, columnFilters)
{
  ret <- lapply(seq_len(ncol(tableToShow)+1), function(x) NULL)
  for(i in seq_along(columnFilters))
  {
    ret[[which(colnames(tableToShow)==names(columnFilters)[i])+1]] <- list(searchf=columnFilters[[i]])
  }
  ret
}

tableLoad <- function(filePath, fileType="rds") 
{
  message("loading ", filePath, "...")
  if(fileType=="rds") 
  {
    dtContent <- data.table(readRDS(filePath))
  }else {
    if(fileType=="rdata") 
    {
      objectName <- load(filePath)
      dtContent <- eval(as.name(objectName))
      rm(list=objectName)
    }else{
      stop("'fileType' not valid")
    }
  }
  message("Table size: ", paste(dim(dtContent), collapse=" x "))
  return(dtContent)
}

tableRender <- function(dtContent,
                        columnTooltip=NULL,
                        columnFilters=NULL) 
{
  ## For tooltip:
  headerCallback <- NULL
  if(!is.null(columnTooltip))  headerCallback <- JS(paste0(
    "function(thead, data, start, end, display){",
    "  var tooltips = ", columnTooltip,";",
    "  for(var i=0; i<=", ncol(dtContent),"; i++){",
    "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
    "  }",
    "}"
  ))
  tbl <- DT::renderDataTable(dtContent, 
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
                               , searchCols = formatColumnFilters(head(dtContent), columnFilters)
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
                                 columnTooltip=NULL,
                                 columnFilters=NULL,
                                 tablesAlreadyLoaded="") 
{
  message("already loaded:", tablesAlreadyLoaded, sep=", ")
  if(!session$ns(NULL) %in% tablesAlreadyLoaded)
  {
    message("Loading ",session$ns(NULL))
    # Do not bookmark table
    donotBookmark <- c(
      # paste0(session$ns(NULL), "-load"), # load button
      paste0(session$ns(NULL), "-tbl_rows_all"), # Needs to be ignored, it is huge!
      paste0(session$ns(NULL), "-tbl_search"),
      paste0(session$ns(NULL), "-tbl_search_columns"),
      paste0(session$ns(NULL), "-tbl_state"),
      paste0(session$ns(NULL), "-tbl_cell_clicked"),
      paste0(session$ns(NULL), "-tbl_rows_selected"),
      paste0(session$ns(NULL), "-tbl_row_last_clicked"),
      paste0(session$ns(NULL), "-tbl_rows_current")## maybe can be kept?
    ) 
    setBookmarkExclude(names=donotBookmark) # TODO tables & anything that is too big...
    
    ### Start loading...
    dtContent <- tableLoad(filePath=filePath, fileType=fileType)
    output$tbl <- tableRender(dtContent, columnTooltip=columnTooltip, columnFilters=columnFilters)
  }
  
  tablesAlreadyLoaded <- unique(c(tablesAlreadyLoaded, session$ns(NULL)))
  return(tablesAlreadyLoaded)
}

# observeEvent(input$load, {
# removeUI(
#   selector = paste0("#", session$ns(NULL),"-load")
# ) 
### isolate(print(reactiveValuesToList(input$load)))
# }
