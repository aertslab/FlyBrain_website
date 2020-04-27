page_stats <- fluidPage(
  h2("Overall statistics of the dataset"),
  br(),
  includeMarkdown("md/stats.Rmd")
)