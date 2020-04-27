page_home <- fluidPage(
  
  ########
  # includeMarkdown("md/homepage.Rmd"), 
  # replace by markdown instead?: 
  h2("The developing fly brain"),
  img(src="img/development.png", width="60%"), 
  h3("Data availability: "),
  ########
  
  
  ## This needs to stay in the _ui.R:
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
      "(screenshot)",
      # br(),
      "-> genome landscape (by cell-type aggregates)",
      # includeMarkdown("md/introToDataset_ucscBox.Rmd")
    ),
    box(
      # title = span(img(src="img/ucsc_icon.png"), "This website"),
      title =  "This website",
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      # "Box content here",
      # br(),
      "details for cell types and regions...",
      # includeMarkdown("md/introToDataset_ucscBox.Rmd")
    )
  )
)