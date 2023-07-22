# -------------------- menu3 -------------------- #

observeMenu3 <- function(input, output, session) {
  
  # render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem(
        "Input data", 
        tabName = "menu1",
        icon = icon("file-import")
      ),
      menuItem(
        "Brain mask",
        tabName = "menu2",
        icon = icon("sliders-h")
      ),
      menuItem(
        "Analysis settings",
        tabName = "menu3",
        icon = icon("cogs")
      )
    )
  })
  isolate({updateTabItems(session, "tabs", "menu3")})
  
  # render setting box (UI)
  output$setBox <- renderUI({
    box(
      title = "Analysis Settings",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      numericInput("alpha", label = "Significance level", 
                   value = 0.05, min = 0, max = 1),
      tags$div(
        style = "margin-bottom: -1em", 
        selectInput("conn", "Connectivity criterion", 
                    choices = c(6, 18, 26), 
                    selected = 18, multiple = FALSE)
      ),
      helpText("Face connectivity (6), edge connectivity (18),
                vertex connectivity (26)."),
      selectInput("simes", "Local test", 
                  choices = c("Simes", "Robust variant of Simes"), 
                  selected = "Simes", multiple = FALSE),
      #textInput("path", "Please specify the full path to output directory:",
      #          value = "/Users/xuchen/Documents/work/LUMC/Rshiny/ARIbrain/app"),
      #/my/path/outputDirectory
      tags$div(
        style = "margin-bottom: -1em", 
        selectInput("twosidedTest", "Statistical test", 
                    choices = c("One-sided test", "Two-sided test"), 
                    selected = "Two-sided test", multiple = FALSE)
      ),
      helpText("Please choose one-sided test if one-sided p-values 
                should be used in statistical analysis."),
    )
  })
  
  # render button box (UI)
  output$runBox <- renderUI({
    box(
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      tags$div(
        style = "display: inline-block;", 
        actionButton("runButton", "To run analysis >>",
                     style = "background-color: #3c8dbc; 
                              color: #fff; 
                              border-color: #717878;")
      ),
      tags$div(
        style = "display: inline-block; width: 1em;",
        HTML("<br>")
      ),
      tags$div(
        style = "display: inline-block;", 
        helpText("Note: the above settings cannot be changed
                  after pressing this button.")
      )
    )
  })
  
  # # observe event after 
  # observeEvent(input$path, {
  #   fileInfo$outputDir <<- input$path
  # })
  
  # observe event after user pressing the button
  observeEvent(input$runButton, {
    shinyjs::disable("alpha")
    shinyjs::disable("conn")
    shinyjs::disable("simes")
    shinyjs::disable("twosidedTest")
    shinyjs::disable("runButton")
  })
  
}

makeMenu3 <- function(input, output, session) {
  
  # observe event after pressing the button
  observeEvent(input$toAnalysisButton, {
    observeMenu3(input, output, session)
  })
  
}

