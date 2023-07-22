# -------------------- menu2 -------------------- #

observeMenu2 <- function(input, output, session) {
  
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
      )
    )
  })
  isolate({updateTabItems(session, "tabs", "menu2")})

  # render checkbox (UI)
  output$autoBox <- renderUI({
    box(
      title = "Import Brain Mask",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      height = "100%",
      checkboxInput(
        "autoMask",
        label = "Brain mask is auto-determined from the non-null & 
                 non-zero input data. (Please uncheck the box if you 
                 want to import the brain mask.)",
        value = TRUE
      ),
      tags$div(
        style = "display: inline-block;",
        actionButton("toAnalysisButton", "To analysis settings >>",
                     style = "background-color: #3c8dbc;
                              color: #fff;
                              border-color: #717878;"),
        tags$div(
          style = "display: inline-block; width: 1em;",
          HTML("<br>")
        ),
        tags$div(
          style = "display: inline-block;",
          helpText("Note: brain mask cannot be changed 
                    after pressing this button.")
        )
      )
    )
  })
  
  # create reactive values
  xyz <- reactiveValues(
    x = round((fileInfo$header$dim[2]+1)/2),
    y = round((fileInfo$header$dim[3]+1)/2),
    z = round((fileInfo$header$dim[4]+1)/2),
    mask = fileInfo$mask
  )

  # observe event after user unchecking the checkbox
  observeEvent(input$autoMask, {

    if(input$autoMask == FALSE) {
      shinyjs::enable("maskFile")
      shinyjs::disable("toAnalysisButton")
      output$maskBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 6,
          fileInput("maskFile", label = "Load mask file")
        )
      })
    } else {
      shinyjs::disable("maskFile")
      shinyjs::enable("toAnalysisButton")
      output$maskBox <- NULL
      # update brain mask
      xyz$mask       <- (!is.na(fileInfo$data)) & (fileInfo$data!=0)
      fileInfo$mask <<- (!is.na(fileInfo$data)) & (fileInfo$data!=0)
    }
  })

  # observe event after loading brain mask
  observeEvent(input$maskFile, {

    file.rename(
      input$maskFile$datapath, paste0(
        dirname(input$maskFile$datapath),
        .Platform$file.sep, input$maskFile$name
      )
    )
    
    mask <- try(
      suppressWarnings(
        RNifti::readNifti(
          paste0(
            dirname(input$maskFile$datapath),
            .Platform$file.sep, input$maskFile$name
          )
        )
      ), silent = TRUE
    )
    
    if(class(mask)[1] == "try-error") {
      shinyjs::disable("toAnalysisButton")
      showModal(
        modalDialog(
          title = "Invalid File Type",
          "You selected an invalid file type."
        )
      )
      return(NULL)
    }
    
    header <- RNifti::niftiHeader(mask)
    
    if(header$dim[1] != 3) {
      shinyjs::disable("toAnalysisButton")
      showModal(
        modalDialog(
          title = "Invalid Dimensions",
          "Nifti-file has invalid dimensions."
        )
      )
      return(NULL)
    }

    if(any(header$dim[2:4] != fileInfo$header$dim[2:4])) {
      shinyjs::disable("toAnalysisButton")
      showModal(
        modalDialog(
          title = "Inconsistent Dimensions",
          "Brain mask & input data have different dimensions."
        )
      )
      return(NULL)
    }
    
    shinyjs::enable("toAnalysisButton")
    # update brain mask
    xyz$mask       <- (!is.na(fileInfo$data)) & (mask!=0)
    fileInfo$mask <<- (!is.na(fileInfo$data)) & (mask!=0)
  })
  
  # observe event after pressing the button
  observeEvent(input$toAnalysisButton, {
    
    shinyjs::disable("autoMask")
    
    if (input$autoMask == FALSE) {
      shinyjs::disable("maskFile")
    }
    
    shinyjs::disable("toAnalysisButton")
  })

  # render image boxes (sagittal, coronal & axial views) (UI)
  output$sagBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("slider_x", label = NULL, step = 1,
                  min = 1, max = fileInfo$header$dim[2],
                  value = xyz$x),
      plotOutput("maskSagittal", click = "click_sag_yz")
    )
  })
  
  output$corBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("slider_y", label = NULL, step = 1,
                  min = 1, max = fileInfo$header$dim[3],
                  value = xyz$y),
      plotOutput("maskCoronal", click = "click_cor_xz")
    )
  })
  
  output$axiBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("slider_z", label = NULL, step = 1,
                  min = 1, max = fileInfo$header$dim[4],
                  value = xyz$z),
      plotOutput("maskAxial", click = "click_axi_xy")
    )
  })
  
  # update mask plots
  output$maskSagittal <- renderPlot({
    plotImage(xyz$mask, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1), 
              FALSE, zlim = c(0,1), views = c("sag"))
    abline(h = xyz$z, v = xyz$y, col = "green")
  })
  output$maskCoronal <- renderPlot({
    plotImage(xyz$mask, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1),
              FALSE, zlim = c(0,1), views = c("cor"))
    abline(h = xyz$z, v = xyz$x, col = "green")
  })
  output$maskAxial <- renderPlot({
    plotImage(xyz$mask, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1),
              FALSE, zlim = c(0,1), views = c("axi"))
    abline(h = xyz$y, v = xyz$x, col = "green")
  })

  # observe event after user clicking the image
  observeEvent(input$click_sag_yz, {
    xyz$y <- round(input$click_sag_yz$x)
    xyz$z <- round(input$click_sag_yz$y)
  })
  observeEvent(input$click_cor_xz, {
    xyz$x <- round(input$click_cor_xz$x)
    xyz$z <- round(input$click_cor_xz$y)
  })
  observeEvent(input$click_axi_xy, {
    xyz$x <- round(input$click_axi_xy$x)
    xyz$y <- round(input$click_axi_xy$y)
  })

  # observe event after user changing the sliders
  observeEvent(input$slider_x, {
    xyz$x <- input$slider_x
  })
  observeEvent(input$slider_y, {
    xyz$y <- input$slider_y
  })
  observeEvent(input$slider_z, {
    xyz$z <- input$slider_z
  })

}

makeMenu2 <- function(input, output, session) {
  
  # observe event after pressing the button
  observeEvent(input$toMaskButton, {
    observeMenu2(input, output, session)
  })
  
}
