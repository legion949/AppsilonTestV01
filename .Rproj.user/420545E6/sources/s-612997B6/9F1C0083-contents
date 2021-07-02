
# Load libraries, functions, global options, etc...
source("load.R")


pageWithSidebar(
  headerPanel('Appsilon Test 01 - Sea Port Work'),
  sidebarPanel(width = 2,

    uiOutput("CompleteSideBar")
  ),
  mainPanel(
    tabsetPanel(id = 'GameMaster',
      tabPanel(title = "Load Data",  uiOutput("contents01")),
      tabPanel(title = "My Ship", uiOutput("contents02")),
      tabPanel(title = "All Records of My Ship", uiOutput("contents03")),
      tabPanel(title = "Longest Distance of My Ship", uiOutput("contents04")),
      tabPanel(title = "Automatic Error Detection on DataSet", uiOutput("contents05"))
    ),
  )
)