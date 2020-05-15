page_home <- fluidPage(

  ########
  # TODO: include in markdown (need to figure out how to set figure size)
    h2("The developing fly brain"),

    h3("Mapping cell types and gene regulatory networks in the fly brain"),
    br(),
    fluidRow(
       column(4, img(src="img/project.png", width = "95%", height="95%")),
       column(4, img(src="img/global_umap.png", width = "91%", height="91%")),
       column(4, img(src="img/tsne.png", width = "90%", height="90%"))
    ),
    br(),
    h3("An interactive platform to explore our single cell datasets"),
    br(),
    includeMarkdown("md/home.Rmd"),
    br(),
  
    h3("Data availability & visualizations"),
    br(),

  ########

  ## This needs to stay in the _ui.R:
  fluidRow(
    box(
      title = span( img(src="img/scope_icon.png"), "Scope"), # TODO: scope icon
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      "Short text description:",
      br(), 
      br(),
      "Use Scope to explore the heterogeneity in region accessibility between cells, 
      compare chromatin accessibility between cell types and/or regions ...",
      br(),
      br(),
      a("http://scope.aertslab.org/#/Fly_Brain", href="http://scope.aertslab.org/#/Fly_Brain", target="_blank"),
      br(),
      br(),
      img(src="img/scope_ss.png", width="100%")
      # includeMarkdown("md/introToDataset_scopeBox.Rmd")
    ),
    box(
      title = span(img(src="img/ucsc_icon.png"), "Genome browser"),
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      includeMarkdown("md/home_ucscBox.Rmd"),
      br(),
      img(src="img/ucsc.png", width="100%")
    )
  )
)