library(data.table)
### Prepare components ----

# # Simple table, without load button:
# defaultTable.ui <- function(id){ 
#   ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
#   DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
# }

# Load table on button press:
tableLoad.ui <- function(id){ 
  ns <- NS(id) # Creates a namespace with the "id" (always the first argument of the function)
  
  fluidPage(
    actionButton(ns("load"), "Show table"), 
    DT::dataTableOutput(ns("tbl"))
  )
  
  # TODO
  # Without the conditional pannel the "spinner" is already there before the button press
  # How to show it conditionally?!?!  (is it needed? - e.g. is it slow?)
  # conditionalPanel(
  #   condition = paste0("output.", ns("clicked")), # "output.tbl_darCellTypes-clicked"
  #   DT::dataTableOutput(ns("tbl")) %>% withSpinner(color="#0dc5c1")
  # )
}

# ### Build page ----
# page_tablesAvailable <- fluidPage(
#   
#   
# )
