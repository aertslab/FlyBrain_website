# url="http://localhost:24514/p/e373a11f/?_inputs_&page=%22Networks%22&Figures-tab=%223D%22&Tfs-tab=%22dotplot1%22&query-tab=%22Query%22&plots_acc_barplots_nes_expr-tf=%22ey%22&Table=%22CellInfo%22&cellTypes-tab=%22descr%22"

cleanupUrl <- function(url){
  
  # Keep only the tab for the current page: 
  url <- strsplit(url,"&")[[1]]
  baseUrl <- c(url[1], grep("page=", url, value=T))
  tabsSelected <- grep("-tab", url, value=T)
  # remainingSettings <- url[which(!url %in% c(baseUrl,tabsSelected))] # at some point keep only settings for the current tab...
  remainingSettings <- NULL # Other settings dont work (probably because they are included in other functions...)
  
  currentTab <- gsub("%22", "", gsub("page=", "", grep("page=", baseUrl, value=T)))
  currentTab <- grep(currentTab, tabsSelected,value=T)
  
  url <- paste0(c(baseUrl, remainingSettings), collapse="&")
  if(length(currentTab) > 0) url <- paste0(c(baseUrl, currentTab, remainingSettings), collapse="&")
  
  # print(url)
  return(url)
}
