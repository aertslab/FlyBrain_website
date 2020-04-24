query_byRegion.server <- function(input, output, session, # not optional
                                  featherFilePath,
                                # fileType="rds",
                                # columnTooltip=NULL
                                ...) 
{
  observeEvent(input$bnt_submitRegions, 
  {
    regions <- input$txt_regions
    regions <- strsplit(regions, "\n|;| ")[[1]]
    regions <- trimws(regions)
    
    source('libs/convertToDbRegions.R')
    regions <- convertToDbRegions(list(regions), featherFilePath=featherFilePath, verbose=F)
    regions <- unique(gsub("dmel_r6.02__", "", regions[[1]]))
    print(regions)
  })
  # output$tbl_
}
