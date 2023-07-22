# -------------------- menu5 -------------------- #

observeMenu5 <- function(input, output, session) {
  
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
      ),
      menuItem(
        "Initial results",
        tabName = "menu4",
        icon = icon("brain")
      ),
      menuItem(
        "Interactive results",
        tabName = "menu5",
        icon = icon("list-alt")
      )
    )
  })
  isolate({updateTabItems(session, "tabs", "menu5")})
  
  # create reactive values
  vs <- reactiveValues(x = round((fileInfo$header$dim[2]+1)/2),
                       y = round((fileInfo$header$dim[3]+1)/2),
                       z = round((fileInfo$header$dim[4]+1)/2),
                       ids_clus = fileInfo$ids_clus,
                       img_clus = fileInfo$img_clus,
                       img_tdps = fileInfo$img_tdps,
                       tblARI = fileInfo$tblARI,
                       tblXYZ = fileInfo$tblXYZ)
  
  # render TDP bound box (UI)
  output$boundBox <- renderUI({
    box(
      title = "Interactive Results",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      tags$div(style = "margin-bottom: 1em",
               "Please click the image or select a row in the cluster 
                table for choosing a cluster. To increase the size of 
                the chosen cluster please press buttons \"+\", and \"-\"
                to decrease the cluster size. Increasing cluster 
                size leads to a reduced TDP, and vice versa.")
      # tags$div(style = "margin-bottom: 1em",
      #          "Please click the image or select a row in the cluster 
      #           table for choosing a cluster. To increase the size of 
      #           the chosen cluster please press buttons \"+\" for normal  
      #           jump or \"++\" for substantial jump, and \"-\" and 
      #           \"--\" to decrease the cluster size. Increasing cluster 
      #           size leads to a reduced TDP, and vice versa.")
    )
  })
  
  # render download button box (UI)
  output$dlBox2 <- renderUI({
    box(
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      tags$div(
        style = "display: inline-block;",
        downloadButton("dlButton2", "Download")
      ),
      tags$div(
        style = "display: inline-block; width: 1em;",
        HTML("<br>")
      ),
      tags$div(
        style = "display: inline-block;",
        helpText("Please press the button to download 
                  the cluster map.")
      )
    )
  })
  
  # Download images
  output$dlButton2 <- downloadHandler(
    filename = "clusimg.nii",
    # function() {
    #   paste0(fileInfo$outputDir, .Platform$file.sep, "clusimg.nii")
    # },
    content = function(file) {
      RNifti::writeNifti(vs$img_clus, file, template = fileInfo$data)
    }
  )
  
  # render sagittal, coronal & axial result boxes (UI)
  output$sagResBox <- renderUI({
    box(
      width = 4,
      height = "100%",
      background = "black",
      sliderInput("xslider", label = NULL, step = 1, value = vs$x,
                  min = 1, max = fileInfo$header$dim[2]),
      plotOutput("imageSag", click = "sag_click")
    )
  })
  output$corResBox <- renderUI({
    box(
      width = 4,
      height = "100%",
      background = "black",
      sliderInput("yslider", label = NULL, step = 1, value = vs$y,
                  min = 1, max = fileInfo$header$dim[3]),
      plotOutput("imageCor", click = "cor_click")
    )
  })
  output$axiResBox <- renderUI({
    box(
      width = 4,
      height = "100%",
      background = "black",
      sliderInput("zslider", label = NULL, step = 1, value = vs$z,
                  min = 1, max = fileInfo$header$dim[4]),
      plotOutput("imageAxi", click = "axi_click")
    )
  })
  
  # observe event after user clicking the image
  observeEvent(input$sag_click, {
    vs$y <- round(input$sag_click$x)
    vs$z <- round(input$sag_click$y)
  })
  observeEvent(input$cor_click, {
    vs$x <- round(input$cor_click$x)
    vs$z <- round(input$cor_click$y)
  })
  observeEvent(input$axi_click, {
    vs$x <- round(input$axi_click$x)
    vs$y <- round(input$axi_click$y)
  })
  
  # observe event after user changing the sliders
  observeEvent(input$xslider, {
    vs$x <- input$xslider
  })
  observeEvent(input$yslider, {
    vs$y <- input$yslider
  })
  observeEvent(input$zslider, {
    vs$z <- input$zslider
  })
  
  # render gradient maps
  output$imageSag <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              vs$x, vs$y, vs$z, gray.colors(64,0,1),
              FALSE, zlim = c(0,1), views = c("sag"))
    plotImage(vs$img_clus, fileInfo$header$dim[2:4], vs$x, vs$y, vs$z, 
              rainbow(max(vs$img_clus[!is.na(vs$img_clus)])), TRUE, 
              zlim = c(1, max(vs$img_clus[!is.na(vs$img_clus)])),
              views = c("sag"))
    abline(h = vs$z, v = vs$y, col = "green")
  })
  output$imageCor <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              vs$x, vs$y, vs$z, gray.colors(64,0,1), 
              FALSE, zlim = c(0,1), views = c("cor"))
    plotImage(vs$img_clus, fileInfo$header$dim[2:4], vs$x, vs$y, vs$z, 
              rainbow(max(vs$img_clus[!is.na(vs$img_clus)])), TRUE, 
              zlim = c(1, max(vs$img_clus[!is.na(vs$img_clus)])),
              views = c("cor"))
    abline(h = vs$z, v = vs$x, col = "green")
  })
  output$imageAxi <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              vs$x, vs$y, vs$z, gray.colors(64,0,1), 
              FALSE, zlim = c(0,1), views = c("axi"))
    plotImage(vs$img_clus, fileInfo$header$dim[2:4], vs$x, vs$y, vs$z, 
              rainbow(max(vs$img_clus[!is.na(vs$img_clus)])), TRUE, 
              zlim = c(1, max(vs$img_clus[!is.na(vs$img_clus)])),
              views = c("axi"))
    abline(h = vs$y, v = vs$x, col = "green")
  })
  
  # render result table box (UI)
  output$tableBox <- renderUI({
    box(
      title = "Cluster Table",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      height = "100%",
      tags$div(
        style = "margin-bottom: 1em", 
        DT::dataTableOutput("resTable")
      ),
      tags$div(style = "display:inline-block", width = 2,
               actionButton("clearRows", "Clear selected cluster")),
      tags$div(style = "display:inline-block", width = 2,
               actionButton("sizePlus",       "Size +")),
      # tags$div(style = "display:inline-block", width = 2,
      #          actionButton("sizePlusPlus",   "Size ++")),
      # tags$div(style = "display:inline-block", width = 2,
      #          actionButton("sizeMinusMinus", "Size --")),
      tags$div(style = "display:inline-block", width = 2,
               actionButton("sizeMinus",      "Size -")),
      tags$div(style = "display:inline-block", width = 2,
               actionButton("redoAnalysis", "Redo", 
                            icon = icon("redo-alt")))
    )
  })
  
  # render ARI result table
  output$resTable <- DT::renderDataTable({
    DT::datatable(
      vs$tblARI,
      options = list(
        columnDefs = list(
          list(className = "dt-center", targets = 1:5)
        ),
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      ),
      escape = FALSE,
      selection = "single",
      editable = "cell"
    ) %>% DT::formatRound(columns = c(3,4), digits = 2)
  })
  
  # observe event after user selecting a row (cluster) in table
  observeEvent(input$resTable_rows_selected, {
    if (!is.null(input$resTable_rows_selected)) {
      if (is.na(vs$img_clus[vs$x, vs$y, vs$z])) {
        vs$x <- vs$tblXYZ[input$resTable_rows_selected,1]
        vs$y <- vs$tblXYZ[input$resTable_rows_selected,2]
        vs$z <- vs$tblXYZ[input$resTable_rows_selected,3] 
      } else if (vs$img_clus[vs$x, vs$y, vs$z] != vs$tblARI[input$resTable_rows_selected,1]) {
        vs$x <- vs$tblXYZ[input$resTable_rows_selected,1]
        vs$y <- vs$tblXYZ[input$resTable_rows_selected,2]
        vs$z <- vs$tblXYZ[input$resTable_rows_selected,3] 
      }
    }
  })
  
  # observe event after pressing button to clear row selection
  DTproxy <- DT::dataTableProxy("resTable", session = session)
  observeEvent(input$clearRows, {
    DT::selectRows(DTproxy, selected = NULL)
    vs$x <- round((fileInfo$header$dim[2]+1)/2)
    vs$y <- round((fileInfo$header$dim[3]+1)/2)
    vs$z <- round((fileInfo$header$dim[4]+1)/2)
  })
  
  # observe event after pressing button to redo the analysis
  observeEvent(input$redoAnalysis, {
    DT::selectRows(DTproxy, selected = NULL)
    vs$x <- round((fileInfo$header$dim[2]+1)/2)
    vs$y <- round((fileInfo$header$dim[3]+1)/2)
    vs$z <- round((fileInfo$header$dim[4]+1)/2)
    vs$ids_clus <- fileInfo$ids_clus
    vs$img_clus <- fileInfo$img_clus
    vs$img_tdps <- fileInfo$img_tdps
    vs$tblARI   <- fileInfo$tblARI
    vs$tblXYZ   <- fileInfo$tblXYZ
    
    shinyjs::enable("sizePlus")
    shinyjs::enable("sizeMinus")
  })
  
  # observe event after changing voxel coordinates
  observeEvent(vs$img_clus[vs$x, vs$y, vs$z], {
    if (!is.na(vs$img_clus[vs$x, vs$y, vs$z])) {
      DT::selectRows(
        DTproxy, selected = which(
          vs$ids_clus == vs$img_clus[vs$x, vs$y, vs$z]
        )
      )
    }
  })
  
  
  # (1) observe event after pressing button to increase size
  observeEvent(input$sizePlus, {
    
    if (!is.na(vs$img_clus[vs$x, vs$y, vs$z])) {

      clus1_label <- vs$img_clus[vs$x, vs$y, vs$z]
      clus1_tdp   <- round(vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 3], 2)
      clus1_size  <- vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2]
      
      # initialize a vector for marking found clusters
      marks   <- integer(fileInfo$m)
      
      while (clus1_tdp > fileInfo$mintdp && 
             clus1_size == vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2]) {
        
        # lower TDP threshold
        clus1_tdp <- clus1_tdp - 0.01
        # find all maximal STCs with a TDP threshold
        clusterlist <- ARIbrain::answerQuery(clus1_tdp, fileInfo$stcs, fileInfo$reslist$SIZE, marks, fileInfo$tdps, fileInfo$reslist$CHILD)
        # sort clusters by descending cluster size
        n <- length(clusterlist)
        if (n > 1) {
          cluster_sizes <- sapply(clusterlist, length)
          d             <- diff(range(cluster_sizes))
          maxsize       <- max(cluster_sizes)
          if (n < 200 || d < 100000 || n*log2(d) <= sum(cluster_sizes)) {
            clusterlist <- clusterlist[order(cluster_sizes, decreasing = TRUE)]
          } else {
            cluster_ord <- ARIbrain:::counting_sort(n, maxsize, cluster_sizes)
            clusterlist <- clusterlist[cluster_ord + 1]
          }
        }
        
        # initialize the cluster map
        clus1_img <- array(0, fileInfo$header$dim[2:4])
        # update cluster map
        for (i in 1:n) {
          clus1_img[fileInfo$indexp[clusterlist[[i]]+1]] <- n-i+1
        }
        
        # update cluster size
        clus1_size <- sum(clus1_img == clus1_img[vs$x, vs$y, vs$z])
      }
      
      if (clus1_tdp <= fileInfo$mintdp) {
        # update cluster table & image
        vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2] <- fileInfo$m
        vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 3] <- fileInfo$mintdp
        vs$img_clus[fileInfo$mask]                    <- clus1_label
        
        shinyjs::disable("sizePlus")
        showModal(
          modalDialog(
            title = NULL,
            "min(TDP) has been reached!"
          )
        )
        return(NULL)
      } else {
        # update cluster table & image
        vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2] <- clus1_size
        vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 3] <- fileInfo$tdps[clusterlist[[n-clus1_img[vs$x, vs$y, vs$z]+1]][clus1_size]+1]
        vs$img_clus[clus1_img == clus1_img[vs$x, vs$y, vs$z]] <- clus1_label
      }
      
    } 
    
  })
  
  # # (2) observe event after pressing button to largely increase size 
  # observeEvent(input$sizePlusPlus, {
  #   
  #   if (!is.na(vs$img_clus[vs$x, vs$y, vs$z])) {
  #     
  #     index1    <- xyz2index(vs$x, vs$y, vs$z, fileInfo$header$dim[2:4])
  #     img_clus  <- vs$img_clus
  #     img_clus[is.na(img_clus)] <- 0
  #     img_clus0 <- ChangeCluster(img_clus, fileInfo$mask, 
  #                                fileInfo$header$dim[2:4], 
  #                                as.numeric(input$conn), input$tdpthres, 
  #                                index1, TRUE, TRUE, fileInfo$outputDir)
  #     
  #     if (length(img_clus0) > 0) {
  #       
  #       img_clus    <- array(img_clus0, fileInfo$header$dim[2:4])
  #       clus_labels <- sort(unique(c(img_clus)))
  #       clus_labels <- clus_labels[clus_labels != 0]
  #       clus_tdps   <- read.delim(paste0(fileInfo$outputDir,
  #                                        .Platform$file.sep, 
  #                                        "sum_clus.txt"),
  #                                 header = FALSE, sep = " ")[,4]
  #       
  #       clus_size <- integer(length(clus_labels))
  #       clus_maxT <- numeric(length(clus_labels))
  #       Vox_xyzs  <- matrix(0, length(clus_labels), 3)
  #       img_tdps  <- array(0, fileInfo$header$dim[2:4])
  #       i <- 1
  #       for (label in clus_labels) {
  #         minp <- min(fileInfo$data[img_clus == label & fileInfo$data > 0])
  #         clus_size[i] <- sum(img_clus == label)
  #         clus_maxT[i] <- -qnorm(minp)
  #         Vox_xyzs[i,] <- which(fileInfo$data == minp, arr.ind = TRUE)
  #         img_tdps[img_clus == label] <- clus_tdps[i]
  #         i <- i + 1
  #       }
  #       
  #       img_clus[img_clus == 0] <- NA
  #       vs$img_clus <- img_clus
  #       vs$img_tdps <- img_tdps
  #       
  #       clus_size   <- sort(clus_size, decreasing = TRUE, index.return = TRUE)
  #       clus_labels <- clus_labels[clus_size$ix]
  #       clus_maxT   <- clus_maxT[clus_size$ix]
  #       Vox_xyzs    <- Vox_xyzs[clus_size$ix,]
  #       MNI_xyzs    <- xyz2MNI(Vox_xyzs, fileInfo$header)
  #       clus_tdps   <- clus_tdps[clus_size$ix]
  #       clus_size   <- clus_size$x
  #       
  #       vs$ids_clus <- clus_labels
  #       
  #       if (length(clus_labels) == 1) {
  #         xyzV <- paste0("(", 
  #                        as.integer(Vox_xyzs[1]), ", ",
  #                        as.integer(Vox_xyzs[2]), ", ",
  #                        as.integer(Vox_xyzs[3]), 
  #                        ")")
  #         xyzM <- paste0("(", 
  #                        as.integer(MNI_xyzs[1]), ", ",
  #                        as.integer(MNI_xyzs[2]), ", ",
  #                        as.integer(MNI_xyzs[3]), 
  #                        ")")
  #       } else {
  #         xyzV <- paste0("(", 
  #                        as.integer(Vox_xyzs[,1]), ", ",
  #                        as.integer(Vox_xyzs[,2]), ", ",
  #                        as.integer(Vox_xyzs[,3]), 
  #                        ")")
  #         xyzM <- paste0("(", 
  #                        as.integer(MNI_xyzs[,1]), ", ",
  #                        as.integer(MNI_xyzs[,2]), ", ",
  #                        as.integer(MNI_xyzs[,3]), 
  #                        ")")
  #       }
  #       
  #       if (!is.null(dim(vs$tblARI)) &
  #           dim(fileInfo$tblARI)[1] - length(clus_labels) > 0) {
  #         
  #         new_label <- integer(dim(fileInfo$tblARI)[1] - 
  #                                length(clus_labels) + 1)
  #         i <- 1
  #         j <- 2
  #         for (label in fileInfo$tblARI[,1]) {
  #           curr_label <- img_clus[fileInfo$tblXYZ[i,1], 
  #                                  fileInfo$tblXYZ[i,2], 
  #                                  fileInfo$tblXYZ[i,3]]
  #           if (curr_label != label) {
  #             new_label[1] <- curr_label
  #             new_label[j] <- label
  #             j <- j + 1
  #           }
  #           i <- i + 1
  #         }
  #         new_label <- sort(new_label)
  #         clus_labels[which(clus_labels == new_label[1])] <- 
  #           paste0(new_label, collapse = ", ")
  #       }
  #       
  #       tblARI <- data.frame(label = clus_labels,
  #                            size = as.integer(clus_size),
  #                            tdps = clus_tdps, 
  #                            maxT = clus_maxT,
  #                            xyzV = xyzV, 
  #                            xyzM = xyzM)
  #       rownames(tblARI) <- paste0("cl", length(clus_labels):1)
  #       colnames(tblARI) <- c("Cluster",
  #                             "Size",
  #                             "TDP",
  #                             "max(Z)",
  #                             "Voxel position<br/>(x, y, z)",
  #                             "MNI coordinates<br/>(x, y, z)")
  #       
  #       vs$tblARI <- tblARI
  #       vs$tblXYZ <- Vox_xyzs
  #       
  #     } else {
  #       
  #       showModal(
  #         modalDialog(
  #           title = NULL,
  #           "min(TDP) has been reached!"
  #         )
  #       )
  #       return(NULL)
  #     }
  #   }
  # })
  # 
  # # (3) observe event after pressing button to largely decrease size
  # observeEvent(input$sizeMinusMinus, {
  #   
  #   if (!is.na(vs$img_clus[vs$x, vs$y, vs$z])) {
  #     
  #     index1    <- xyz2index(vs$x, vs$y, vs$z, fileInfo$header$dim[2:4])
  #     img_clus  <- vs$img_clus
  #     img_clus[is.na(img_clus)] <- 0
  #     img_clus0 <- ChangeCluster(img_clus, fileInfo$mask, 
  #                                fileInfo$header$dim[2:4], 
  #                                as.numeric(input$conn), input$tdpthres, 
  #                                index1, FALSE, TRUE, fileInfo$outputDir)
  #     
  #     if (length(img_clus0) > 0) {
  #       
  #       img_clus    <- array(img_clus0, fileInfo$header$dim[2:4])
  #       clus_labels <- sort(unique(c(img_clus)))
  #       clus_labels <- clus_labels[clus_labels != 0]
  #       # clus_tdps   <- read.delim(paste0(fileInfo$outputDir,
  #       #                                  .Platform$file.sep, 
  #       #                                  "sum_clus.txt"),
  #       #                           header = FALSE, sep = " ")[,4]
  #       
  #       clus_size <- integer(length(clus_labels))
  #       clus_maxT <- numeric(length(clus_labels))
  #       Vox_xyzs  <- matrix(0, length(clus_labels), 3)
  #       img_tdps  <- array(0, fileInfo$header$dim[2:4])
  #       i <- 1
  #       for (label in clus_labels) {
  #         minp <- min(fileInfo$data[img_clus == label & fileInfo$data > 0])
  #         clus_size[i] <- sum(img_clus == label)
  #         clus_maxT[i] <- -qnorm(minp)
  #         Vox_xyzs[i,] <- which(fileInfo$data == minp, arr.ind = TRUE)
  #         img_tdps[img_clus == label] <- clus_tdps[i]
  #         i <- i + 1
  #       }
  #       
  #       img_clus[img_clus == 0] <- NA
  #       vs$img_clus <- img_clus
  #       vs$img_tdps <- img_tdps
  #       
  #       clus_size   <- sort(clus_size, decreasing = TRUE, index.return = TRUE)
  #       clus_labels <- clus_labels[clus_size$ix]
  #       clus_maxT   <- clus_maxT[clus_size$ix]
  #       Vox_xyzs    <- Vox_xyzs[clus_size$ix,]
  #       MNI_xyzs    <- xyz2MNI(Vox_xyzs, fileInfo$header)
  #       clus_tdps   <- clus_tdps[clus_size$ix]
  #       clus_size   <- clus_size$x
  #       
  #       vs$ids_clus <- clus_labels
  #       
  #       if (length(clus_labels) == 1) {
  #         xyzV <- paste0("(", 
  #                        as.integer(Vox_xyzs[1]), ", ",
  #                        as.integer(Vox_xyzs[2]), ", ",
  #                        as.integer(Vox_xyzs[3]), 
  #                        ")")
  #         xyzM <- paste0("(", 
  #                        as.integer(MNI_xyzs[1]), ", ",
  #                        as.integer(MNI_xyzs[2]), ", ",
  #                        as.integer(MNI_xyzs[3]), 
  #                        ")")
  #       } else {
  #         xyzV <- paste0("(", 
  #                        as.integer(Vox_xyzs[,1]), ", ",
  #                        as.integer(Vox_xyzs[,2]), ", ",
  #                        as.integer(Vox_xyzs[,3]), 
  #                        ")")
  #         xyzM <- paste0("(", 
  #                        as.integer(MNI_xyzs[,1]), ", ",
  #                        as.integer(MNI_xyzs[,2]), ", ",
  #                        as.integer(MNI_xyzs[,3]), 
  #                        ")")
  #       }
  #       tblARI <- data.frame(label = as.integer(clus_labels),
  #                            size = as.integer(clus_size),
  #                            tdps = clus_tdps, 
  #                            maxT = clus_maxT,
  #                            xyzV = xyzV, 
  #                            xyzM = xyzM)
  #       rownames(tblARI) <- paste0("cl", 1:length(clus_labels))
  #       colnames(tblARI) <- c("Local maximum",
  #                             "Size",
  #                             "TDP",
  #                             "max(Z)",
  #                             "Voxel position<br/>(x, y, z)",
  #                             "MNI coordinates<br/>(x, y, z)")
  #       
  #       vs$tblARI <- tblARI
  #       vs$tblXYZ <- Vox_xyzs
  #       
  #     } else {
  #       
  #       showModal(
  #         modalDialog(
  #           title = NULL,
  #           "min(TDP) has been reached!"
  #         )
  #       )
  #       return(NULL)
  #     }
  #   }
  # })
  
  # (4) observe event after pressing button to decrease size
  observeEvent(input$sizeMinus, {
    
    if (!is.na(vs$img_clus[vs$x, vs$y, vs$z])) {
      
      clus1_label <- vs$img_clus[vs$x, vs$y, vs$z]
      clus1_tdp   <- round(vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 3], 2)
      clus1_size  <- vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2]
      
      # initialize a vector for marking found clusters
      marks <- integer(fileInfo$m)
      
      while (clus1_tdp < 1 && clus1_size == vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2]) {
        
        # increase TDP threshold
        clus1_tdp <- clus1_tdp + 0.01
        # find all maximal STCs with a TDP threshold
        clusterlist <- ARIbrain::answerQuery(clus1_tdp, fileInfo$stcs, fileInfo$reslist$SIZE, marks, fileInfo$tdps, fileInfo$reslist$CHILD)
        # sort clusters by descending cluster size
        n <- length(clusterlist)
        if (n > 1) {
          cluster_sizes <- sapply(clusterlist, length)
          d             <- diff(range(cluster_sizes))
          maxsize       <- max(cluster_sizes)
          if (n < 200 || d < 100000 || n*log2(d) <= sum(cluster_sizes)) {
            clusterlist <- clusterlist[order(cluster_sizes, decreasing = TRUE)]
          } else {
            cluster_ord <- ARIbrain:::counting_sort(n, maxsize, cluster_sizes)
            clusterlist <- clusterlist[cluster_ord + 1]
          }
        }
        
        # initialize the cluster map
        clus1_img <- array(0, fileInfo$header$dim[2:4])
        # update cluster map
        for (i in 1:n) {
          clus1_img[fileInfo$indexp[clusterlist[[i]]+1]] <- n-i+1
        }
        
        # update cluster size
        clus1_size <- sum(clus1_img == clus1_img[vs$x, vs$y, vs$z])
      }
      
      # update cluster table & image
      vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 2] <- clus1_size
      vs$tblARI[dim(vs$tblARI)[1]-clus1_label+1, 3] <- fileInfo$tdps[clusterlist[[n-clus1_img[vs$x, vs$y, vs$z]+1]][clus1_size]+1]
      vs$img_clus[clus1_img == clus1_img[vs$x, vs$y, vs$z]] <- clus1_label
      
      if (clus1_tdp >= 1) {
        shinyjs::disable("sizeMinus")
        showModal(
          modalDialog(
            title = NULL,
            "max(TDP) = 1 has been reached!"
          )
        )
        return(NULL)
      }
      
    } 
    
  })
  
}

makeMenu5 <- function(input, output, session) {
  
  # observe event after pressing the button
  observeEvent(input$interButton, {
    observeMenu5(input, output, session)
  })
  
}


