page_introToDataset <- fluidPage(
  includeMarkdown("md/introToDataset_main.Rmd"),
  
  h2("Data access:"),
  fluidRow(
    box(
      title = span( img(src="img/scope_icon.png"), "Scope"), # TODO: scope icon
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      "Short text description...", 
      "Allows to: ", 
      "- See the heterogeneity among cells for a given region",
      "- Compare...",
      br(), 
      a("http://scope.aertslab.org/#/Fly_Brain", href="http://scope.aertslab.org/#/Fly_Brain", target="_blank"),
      br(),
      img(src="img/scope_ss.png", width="100%")
      # includeMarkdown("md/introToDataset_scopeBox.Rmd")
    ), 
    box(
      title = span(img(src="img/ucsc_icon.png"), "Genome browser"),
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      "Box content here",
      br(), 
      "-> genome landscape (by cell-type aggregates)",
      # includeMarkdown("md/introToDataset_ucscBox.Rmd")
    )
  )
)
