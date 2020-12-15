# TODO: Remove, just as example
histPlot.server <- function(input, output, session) {
  
  ## Data is only loaded once (I guess it depends on when the module is called...): 
  histdata <- rnorm(500)  # TODO: Remove, just an example
  
  # nValues <- reactiveVal(0) # initialize
  # observeEvent(input$slider,{
  #   nValues(input$slider) # set value
  # })
  
  output$plot1 <- renderPlot({  
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

library(visNetwork)
sampleNetwork.server <- function(input, output, session) {
  ####### Prepare edge lists:
  set.seed(333)
  m2r <- data.frame(motif=rep(c("homer__ACTGATAAGA_PQM-1","hocomoco__ALX4_HUMAN.H11MO.0.D","transfac_pro__M07784"), 3)[1:4], 
                    region=rep(c("chr:938200129:112211111", "chr:112211111:938200129"), 2))
  m2r <- unique(m2r)
  nrow(m2r)
  
  tf2m <- data.frame(tf=paste0("tf", sample(1:3, length(unique(m2r$motif))*2, replace=T)), motif=rep(unique(m2r$motif), each=2))
  tf2m <- unique(tf2m)
  nrow(tf2m)
  
  r2g <- data.frame(region=rep(unique(m2r$region), each=4), 
                    gene=sample(paste0("gene", 1:8),8, replace=T))
  r2g <- unique(r2g)
  nrow(r2g)
  #######
  
  
  ### Edge properties:
  # dashes
  m2r$dashes <- FALSE
  tf2m$dashes <- TRUE
  r2g$dashes <- TRUE
  
  # m2r$length <- 100
  # tf2m$length <- 200
  # r2g$length <- 200
  
  # Merge into one: 
  edges <- list(m2r, tf2m, r2g)
  edges <- lapply(edges, function(x) { colnames(x)[1:2] <- c("from", "to"); x})
  edges <- do.call(rbind, edges)
  edges <- unique(edges)
  # Common props
  edges$arrows <- "to"
  # edges$label # maybe instead of motif?
  # edges$title # tooltip
  edges$smooth <- FALSE
  # edges$shadow <- TRUE/FALSE
  
  ### Node properties
  ids <- list()
  ids[["motifs"]] <- unique(as.character(m2r[,"motif"]))
  ids[["regions"]] <- unique(as.character(m2r[,"region"]))
  ids[["tfs"]] <- unique(as.character(tf2m[,"tf"]))
  ids[["genes"]] <- unique(as.character(r2g[,"gene"]))
  nodes <- data.frame(id=unname(unlist(ids)))
  nodes$label <- nodes$id
  nodes$title <- nodes$id # tooltip 
  nodes$shape <- c(rep("image", length(ids[["motifs"]])), # image / circularImage / diamond
                   rep("box", length(ids[["regions"]])), 
                   rep("elypse", length(ids[["tfs"]])), 
                   rep("elypse", length(ids[["genes"]])))
  nodes$color <- c(rep("purple", length(ids[["motifs"]])),  
                   rep("orange", length(ids[["regions"]])), 
                   rep("skyblue", length(ids[["tfs"]])), 
                   rep("skyblue", length(ids[["genes"]]))) # red/green
  # nodes$shadow
  logosPath <- "http://motifcollections.aertslab.org/v9/logos/"
  nodes$image <- c(paste0(logosPath, ids[["motifs"]], ".png"),
                   rep("", length(ids[["regions"]])), 
                   rep("", length(ids[["tfs"]])), 
                   rep("", length(ids[["genes"]])))
  # ### Group (I thought it was to collapse, but no...)
  # nodeGroups <- sapply(c(split(as.character(r2g$region), r2g$gene), 
  #                        split(as.character(tf2m$motif), tf2m$tf),
  #                        setNames(ids[["regions"]], ids[["regions"]]),
  #                        setNames(ids[["motifs"]], ids[["motifs"]])), 
  #                      paste, collapse=", ")
  # nodes$group <- nodeGroups[as.character(nodes$id)]
  
  observe({
    hierarchicalLayout <- TRUE  # Change to input
    p <- visNetwork(unique(nodes), edges) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE,
                 collapse = list(enabled = TRUE)) %>%
      visNodes(shapeProperties = list(useBorderWithImage=TRUE, interpolation=FALSE)) %>%
      visLayout(randomSeed = 2)
    if(hierarchicalLayout) p <- p %>% visHierarchicalLayout(direction="UD", sortMethod="directed", nodeSpacing=10)  # does not look nice if TFs are also targets
    output$plot_nwExample <- renderVisNetwork(p)
  })

}