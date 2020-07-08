page_home <- fluidPage(
  includeMarkdown("md/home.Rmd"),
  ## This needs to stay in the _ui.R:
  fluidRow(
    box(
      title = span( img(src="img/scope_icon.png"), "Scope"), # TODO: scope icon
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      "SCope: explore", strong("heterogeneity between individual cells"), "(i.e. compare chromatin accessibility between cell types and/or regions ...)",
      br(),
      br(),
      "URL:", a("http://scope.aertslab.org/#/Fly_Brain", href="http://scope.aertslab.org/#/Fly_Brain", target="_blank"),
      br(),
      br(),
      img(src="img/scope_ss.png", width="100%")
    ),
    box(
      title = span(img(src="img/ucsc_icon.png"), "Genome browser"),
      collapsible = TRUE,
      status="warning", solidHeader=TRUE,
      "UCSC Genome Browser can be used to explore the", strong("aggregates by cell type"), "and the region annotation in their genomic context.",
      br(),
      br(),
      "URL:", a("http://ucsctracks.aertslab.org/papers/FlyBrain/hub.txt", href="http://genome.ucsc.edu/cgi-bin/hgTracks?db=dm6&hubUrl=http://ucsctracks.aertslab.org/papers/FlyBrain/hub.txt", target="_blank"),
      # TO DO: convert to session
      br(),
      br(),
      img(src="img/ucsc_ss.png", width="100%")
    )
  )
)