page_home <- fluidPage(
  includeMarkdown("md/home.Rmd"),
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