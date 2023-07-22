# -------------------- menu1 -------------------- #

makeMenu1 <- function(input, output, session) {
  
  # render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem(
        "Input data",
        tabName = "menu1",
        icon = icon("file-import")
      )
    )
  })
  
  # render data box (UI)
  output$dataBox <- renderUI({
    box(
      title = "Import Data",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      height = "100%",
      fileInput("dataFile", label = "Load statistic file"),
      selectInput("dataType", "File type:",
                  choices = "", selected = ""),
      helpText("ARI allows z, t, and p-maps as input data.",
               "ARI will try to determine which type loaded.",
               "You can also change the file type.")
    )
  })
  
  # render button box (UI)
  output$buttonBox <- renderUI({
    box(
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      tags$div(
        style = "display: inline-block;", 
        actionButton("toMaskButton", "To import mask >>",
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
        helpText("Note: input data cannot be changed after 
                  pressing this button.")
      )
    )
  })
  
  # set input & button boxes to disable
  observe({
    shinyjs::toggleState("dataType",     !is.null(input$dataFile))
  })
  observe({
    shinyjs::toggleState("toMaskButton", !is.null(input$dataFile))
  })
  
  # observe event after data import
  observeEvent(input$dataFile, {
    
    fileInfo <<- checkFileType(input$dataFile)
    
    if(!(fileInfo$valid)) {
      shinyjs::disable("dataType")
      shinyjs::disable("toMaskButton")
      showModal(
        modalDialog(
          title = "Invalid File Type",
          "You selected an invalid file type."
        )
      )
      return(NULL)
    }
    
    if(fileInfo$header$dim[1] != 3) {
      shinyjs::disable("dataType")
      shinyjs::disable("toMaskButton")
      showModal(
        modalDialog(
          title = "Invalid Dimensions", 
          "Nifti-file has invalid dimensions."
        )
      )
      return(NULL)
    }
    
    shinyjs::enable("dataType")
    
    if(fileInfo$selected == "unknown") {
      updateSelectInput(
        session = session, "dataType",
        choices = c("Please choose", "z-map", "t-map", "p-map"), 
        selected = "Please choose"
      )  
    } else {
      updateSelectInput(
        session = session, "dataType",
        choices = c("z-map", "t-map", "p-map"), 
        selected = fileInfo$selected
      )
    }
    
  })
  
  # observe event after selecting data type
  observeEvent(input$dataType, {
    
    if(input$dataType == "z-map") {
      fileInfo$selected <<- "z-map"
      fileInfo$type <<- "z"
      output$infoBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          title = "Z-map", width = 6,
          "Z-statistics are used for the ARI analysis."
        )
      })
      shinyjs::enable("toMaskButton")
      updateSelectInput(
        session = session, "dataType",
        choices = c("z-map", "t-map", "p-map"), 
        selected = fileInfo$selected
      )  
    }
    
    if(input$dataType == "t-map") {
      fileInfo$selected <<- "t-map"
      fileInfo$type <<- "t"
      output$infoBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          title = "T-map", width = 6,
          numericInput("tdf",
                       "Please enter degrees of freedom of t-stats:",
                       value = fileInfo$df),
          "Properly converting t-statistics to p-values requires 
           degrees of freedom. If df = 0, ARI will treat all t-stats 
           as z-values."
        )
      })
      shinyjs::enable("toMaskButton")
      updateSelectInput(
        session = session, "dataType",
        choices = c("z-map", "t-map", "p-map"), 
        selected = fileInfo$selected
      )  
    } 
    
    if(input$dataType == "p-map") {
      fileInfo$selected <<- "p-map"
      fileInfo$type <<- "p"
      output$infoBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          title = "P-map", width = 6,
          selectInput("twosided", "Input p-values:",
                      choices = c("one-sided", "two-sided"), 
                      selected = "two-sided"),
          "ARI assumes p-values are two-sided. One-sided p-values 
           will be transformed to two-sided (unless one-sided is 
           selected at the analysis settings)."
        )
      })
      shinyjs::enable("toMaskButton")
      updateSelectInput(
        session = session, "dataType",
        choices = c("z-map", "t-map", "p-map"), 
        selected = fileInfo$selected
      )  
    }
    
    if(input$dataType == "Please choose") {
      fileInfo$selected <<- "unknown"
      fileInfo$type <<- "u"
      output$infoBox <- renderUI({
        box(
          solidHeader = TRUE,
          title = "Unknown file type", width = 6,
          "ARI was unable to determine file-type. Please select type
           of statistics file."
        )
      })
      shinyjs::disable("toMaskButton")
    }
  })
  
  # observe event after pressing the button
  observeEvent(input$toMaskButton, {
    
    shinyjs::disable("dataFile")
    shinyjs::disable("dataType")
    
    if(input$dataType == "p-map") {
      shinyjs::disable("twosided")
    }
    
    if(input$dataType == "t-map") {
      shinyjs::disable("tdf")
    }
    
    shinyjs::disable("toMaskButton")
  })
  
}
