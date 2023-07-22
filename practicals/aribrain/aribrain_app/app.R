## --------------- (0) Preparation --------------- ##

# By default, Shiny limits file uploads to 5MB per file. This limit is modified
# by using the shiny.maxRequestSize option to 100MB.
options(shiny.maxRequestSize = 100*1024^2)

# load libraries
packages <- c("shiny", 
              "shinyjs",
              "shinydashboard",
              "DT",
              "RNifti", 
              "Rcpp",
              "hommel",
              "ARIbrain")
pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(pkgs)) {
  install.packages(pkgs, dependencies = TRUE) 
}
suppressPackageStartupMessages(
  invisible(sapply(packages, require, character.only = TRUE))
)

# source necessary scripts
source("utils.R")
source("menu1.R")
source("menu2.R")
source("menu3.R")
source("menu4.R")
source("menu5.R")

# define package version
appVersion <- "0.1.2"

## --------------- (1) Define UI --------------- ##
## using "shinydashboard" ##

# define header
header <- dashboardHeader(
  title = "ARIbrain"
  #title = paste0("ARIbrain v", appVersion)
)

# define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))
)

# define body
body <- dashboardBody(
  
  ## using "shinyjs" ##
  shinyjs::useShinyjs(),
  
  tabItems(
    # 1st tab content
    tabItem(
      tabName = "menu1",
      fluidRow(
        uiOutput("dataBox"),
        uiOutput("infoBox")
      ),
      fluidRow(
        uiOutput("buttonBox")
      )
    ),
    
    # 2nd tab content
    tabItem(
      tabName = "menu2",
      fluidRow(
        uiOutput("autoBox"),
        uiOutput("maskBox")
      ),
      fluidRow(
        uiOutput("sagBox"),
        uiOutput("corBox"),
        uiOutput("axiBox")
      )
    ),
    
    # 3rd tab content
    tabItem(
      tabName = "menu3",
      fluidRow(
        uiOutput("setBox")
      ),
      fluidRow(
        uiOutput("runBox")
      )
    ),
    
    # 4th tab content
    tabItem(
      tabName = "menu4",
      fluidRow(
        uiOutput("thresBox"),
        uiOutput("cftBox")
      ),
      fluidRow(
        uiOutput("sagResultBox"),
        uiOutput("corResultBox"),
        uiOutput("axiResultBox")
      ),
      fluidRow(
        uiOutput("dlBox")
      ),
      fluidRow(
        uiOutput("interBox")
      )
    ),
    
    # 5th tab content
    tabItem(
      tabName = "menu5",
      fluidRow(
        uiOutput("boundBox")
      ),
      fluidRow(
        uiOutput("sagResBox"),
        uiOutput("corResBox"),
        uiOutput("axiResBox")
      ),
      fluidRow(
        uiOutput("dlBox2")
      ),
      fluidRow(
        uiOutput("tableBox")
      )
    )
  )
)

# define UI
ui <- dashboardPage(header, sidebar, body)


## --------------- (2) Define server logic --------------- ##
server <- function(input, output, session) {
  
  # set global variables
  fileInfo <<- NULL
  
  makeMenu1(input, output, session)
  
  makeMenu2(input, output, session)
  
  makeMenu3(input, output, session)
  
  makeMenu4(input, output, session)
  
  makeMenu5(input, output, session)
}


## --------------- (3) Run the app --------------- ##
shinyApp(ui = ui, server = server)
# app <- shinyApp(ui, server)
# runApp(app)
