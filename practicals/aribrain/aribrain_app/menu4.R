# -------------------- menu4 -------------------- #

observeMenu4 <- function(input, output, session) {
  
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
      )
    )
  })
  isolate({updateTabItems(session, "tabs", "menu4")})
  
  # create reactive values
  xyz <- reactiveValues(
    x = round((fileInfo$header$dim[2]+1)/2),
    y = round((fileInfo$header$dim[3]+1)/2),
    z = round((fileInfo$header$dim[4]+1)/2),
    img_tdps = array(0, fileInfo$header$dim[2:4]),
    img_clus = NULL
  )
  
  # render threshold box (UI)
  output$thresBox <- renderUI({
    box(
      title = "Initial Results",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      height = "100%",
      tags$div(
        style = "margin-bottom: 1em",
        "The gray-scale background image below is a TDP gradient map 
         showing the maximum true discovery proportion (TDP) each voxel 
         can reach. You can download it by pressing the download button. 
         To generate clusters please select a test statistic threshold or 
         a TDP threshold."
      ),
      tags$div(
        style = "margin-bottom: -1em", 
        selectInput(
          "CFTthres", "Cluster-forming threshold:", 
          choices = c("Please choose", "stats", "TDP"), 
          selected = "Please choose", multiple = FALSE
        )
      )
    )
  })
  
  # render to interactive clustering button box (UI)
  output$interBox <- renderUI({
    box(
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      height = "100%",
      tags$div(
        style = "display: inline-block;",
        shinyjs::disabled(
          actionButton(
            "interButton", "To interactive analysis >>",
            style = "background-color: #3c8dbc; 
                     color: #fff; border-color: #717878;")
        )
      ),
      tags$div(
        style = "display: inline-block; width: 1em;",
        HTML("<br>")
      ),
      tags$div(
        style = "display: inline-block;",
        helpText("Please press the button for an interactive thresholding, 
                  where you can change the size of each cluster.")
      )
    )
  })
  
  
  # ------------------------------------------------------ #
  # ---------- (below) run ARI analysis (below) ---------- #
  withProgress(message = NULL, value = 0, {
    
    setProgress(message = "Doing preparation.",
                detail  = "This may take a few seconds...")
    
    # check input parameter types
    if (input$simes == "Simes") {
      simes <- TRUE
    } else {
      simes <- FALSE
    }
    conn  <- as.integer(input$conn)
    alpha <- as.numeric(input$alpha)
    
    # compute p-values
    pval <- fileInfo$data
    if(input$twosidedTest == TRUE) { # two-sided test
      
      if(fileInfo$type == "p") {
        pval[fileInfo$mask == 0] <- 1
        if(input$twosided == FALSE) { pval <- 2 * pval }
      } else if(fileInfo$type == "t") {
        pval[fileInfo$mask == 0] <- 0
        if (input$tdf > 0) {
          pval <- 2 * pt(-abs(pval), df = input$tdf)
        } else if (input$tdf == 0) {
          pval <- 2 * pnorm(-abs(pval))
        }
      } else if(fileInfo$type == "z") {
        pval[fileInfo$mask == 0] <- 0
        pval <- 2 * pnorm(-abs(pval))
      }
      
    } else { # one-sided test
      
      if(fileInfo$type == "p") {
        pval[fileInfo$mask == 0] <- 1
        if(input$twosided == TRUE) { pval <- pval/2 }
      } else if(fileInfo$type == "t") {
        pval[fileInfo$mask == 0] <- 0
        if (input$tdf > 0) {
          pval <- 1 - pt(pval, df = input$tdf)
        } else if (input$tdf == 0) {
          pval <- 1 - pnorm(pval)
        }
      } else if(fileInfo$type == "z") {
        pval[fileInfo$mask == 0] <- 0
        pval <- 1 - pnorm(pval)
      }
      
    }
    pval <- pmin(1, pval)
    pval <- pmax(0, pval)
    
    # get voxel indices of the mask in 3D space
    indexp <- (1:length(fileInfo$mask))[fileInfo$mask != 0] # indexp <- which(fileInfo$mask != 0)
    # extract p-values of in-mask voxels
    p      <- pval[indexp]
    # compute size of the multiple testing problem
    m      <- length(indexp)
    
    # compute the whole-brain TDP, i.e. min(TDP)
    hom    <- hommel::hommel(p, simes = simes)
    mintdp <- hommel::tdp(hom, alpha = alpha)
    # return warning if there are no activations
    if(mintdp == 0) {
      shinyjs::disable("CFTthres")
      shinyjs::disable("dlButton")
      showModal(
        modalDialog(
          title = "No Activations",
          "No significant brain activations can be detected."
        )
      )
      return(NULL)
    }
    
    # compute h
    halpha      <- hommel:::findHalpha(hom@jumpalpha, alpha = alpha, m)
    simeshalpha <- hom@simesfactor[halpha+1]
    conc_thres  <- hommel::concentration(hom, alpha = alpha)
    rm(hom)
    
    # sort p-values in ascending order
    ordp    <- order(p)
    ordp    <- as.integer(ordp)   # sorted orders (starts from 1)
    # find the sorting ranks for unsorted p-values
    rankp   <- integer(m)
    rankp[ordp] <- 1:m
    rankp   <- as.integer(rankp)  # sorting ranks (starts from 1)
    
    # create 3D whole-brain mask of unsorted orders (starts from 1)
    maskI   <- array(0, fileInfo$header$dim[2:4])
    maskI[indexp] <- 1:m
    maskI   <- as.integer(maskI)
    
    # find the adjacency list
    adj     <- ARIbrain::findAdjList(maskI, as.integer(indexp-1), fileInfo$header$dim[2:4], m, conn)
    # find all admissible STCs & compute TDP bounds
    reslist <- ARIbrain::findClusters(m, adj, ordp, rankp)
    tdps    <- ARIbrain::forestTDP(m, halpha, alpha, simeshalpha, p, reslist$SIZE, reslist$ROOT, reslist$CHILD)
    stcs    <- ARIbrain::queryPreparation(m, reslist$ROOT, tdps, reslist$CHILD)
    
    # update fileInfo with necessary arguments
    fileInfo$m          <<- m
    fileInfo$mintdp     <<- mintdp
    fileInfo$indexp     <<- indexp
    fileInfo$reslist    <<- reslist
    fileInfo$tdps       <<- tdps
    fileInfo$stcs       <<- stcs
    fileInfo$conc_thres <<- conc_thres
    
    # initialize a vector for marking found clusters
    marks   <- integer(m)
    # initialize cluster image: >0 for voxels within clusters & gradient map
    clusimg <- array(0, fileInfo$header$dim[2:4])
    gradmap <- array(0, fileInfo$header$dim[2:4])
    
    # compute voxel-wise max(TDP) bounds, i.e., TDP gradient map
    gammas <- seq(0, 1, 0.01)  # TDP thresholds
    for (g in gammas) {
      # form clusters
      clusterlist <- ARIbrain::answerQuery(g, stcs, reslist$SIZE, marks, tdps, reslist$CHILD)
      
      out_ids <- indexp[unlist(clusterlist)+1]
      clusimg[out_ids] <- 1
      gradmap <- pmax(gradmap, (clusimg > 0)*g)
      clusimg[out_ids] <- 0
      
      incProgress(1/length(gammas), message = "Running analysis...", 
                  detail = paste0(round(g*100), "% completed"))
    }
    
    # update fileInfo with the gradient map
    fileInfo$grad_map <<- gradmap
  })
  
  # ---------- (above) run ARI analysis (above) ---------- #
  # ------------------------------------------------------ #
  
  # observe event after user selecting CFT
  observeEvent(input$CFTthres, {
    
    if (input$CFTthres == "Please choose") {
      
      shinyjs::disable("interButton")
      xyz$img_clus  <- NULL
      xyz$img_tdps  <- array(0, fileInfo$header$dim[2:4])
      output$cftBox <- NULL
      
      # render download button box (UI)
      output$dlBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          height = "100%",
          tags$div(
            style = "display: inline-block;",
            downloadButton("dlButton", "Download")
          ),
          tags$div(
            style = "display: inline-block; width: 1em;",
            HTML("<br>")
          ),
          tags$div(
            style = "display: inline-block;",
            helpText("Please press the button to download 
                  the TDP gradient map.")
          )
        )
      })
    }
    
    if(input$CFTthres == "stats") {
      
      shinyjs::enable("interButton")
      xyz$img_clus  <- NULL
      xyz$img_tdps  <- array(0, fileInfo$header$dim[2:4])
      
      z_001  <- round(-qnorm(0.001), 2)
      z_conc <- round(-qnorm(fileInfo$conc_thres), 2)
      if(fileInfo$type == "p") {
        z_max  <- round(-qnorm(min(pval[fileInfo$mask!=0])), 2)
      } else {
        z_max  <- round(max(fileInfo$data[fileInfo$mask!=0]), 2)
      }
      
      output$cftBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 6,
          sliderInput(
            "zthres", label = "Z-score threshold", 
            min = min(z_001, z_conc), max = z_max, 
            step = 0.01, value = z_conc
          ),
          helpText("Applying a test statistic threshold leads to conventional 
                    supra-threshold clusters. The created clusters are overlaid 
                    onto the TDP gradient map. Different colors represent different 
                    clusters."),
          actionButton("ZclusButton", "To form clusters >>",
                       style = "background-color: #3c8dbc; 
                                color: #fff; 
                                border-color: #717878;")
        )
      })
      
      output$dlBox <- NULL
    } 
    
    if (input$CFTthres == "TDP") {
      
      shinyjs::enable("interButton")
      xyz$img_clus  <- NULL
      xyz$img_tdps  <- array(0, fileInfo$header$dim[2:4])
      
      output$cftBox <- renderUI({
        box(
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 6,
          sliderInput("tdpthres", label = "TDP threshold",
                      min = round(fileInfo$mintdp, 2), 
                      max = 1, step = 0.01, value = 0.7),
          helpText("Applying a TDP threshold is equivalent to using 
                    different test statistic thresholds for cluster generation. 
                    Each created cluster has the TDP not below the given 
                    threshold. The created clusters are overlaid onto 
                    the TDP gradient map, with different colors for
                    different clusters."),
          actionButton("TDPclusButton", "To form clusters >>",
                       style = "background-color: #3c8dbc; 
                                color: #fff; 
                                border-color: #717878;")
        )
      })
      
      output$dlBox <- NULL
    }
    
  })
  
  # observe event after pressing button to form classical supra-threshold 
  # clusters
  observeEvent(input$ZclusButton, {
    
    withProgress(message = "Calculation in progress.", value = 0,
                 detail  = "This may take a few seconds...", {
                   
                   # create clusters with a test statistic threshold
                   if(fileInfo$type == "p") {
                     img_clus <- ARIbrain::cluster_threshold(
                       pval < pnorm(-input$zthres) & fileInfo$mask != 0
                     )
                   } else {
                     img_clus <- ARIbrain::cluster_threshold(
                       fileInfo$data > input$zthres & fileInfo$mask != 0
                     )
                   }            
                   
                   # find cluster labels
                   clus_labels <- sort(unique(as.vector(img_clus[img_clus>0])), decreasing=TRUE)
                   
                   # run simple ARI analysis
                   if(fileInfo$type == "p") {
                     tblARI <- ARIbrain::ARI(
                       Pmap     = pval,
                       clusters = img_clus,
                       mask     = fileInfo$mask,
                       Statmap  = -qnorm(pval),
                       silent   = TRUE
                     )
                   } else {
                     tblARI <- ARIbrain::ARI(
                       Pmap     = pval,
                       clusters = img_clus,
                       mask     = fileInfo$mask,
                       Statmap  = fileInfo$data,
                       silent   = TRUE
                     )
                   }
                   
                   # extract clusters with positive TDP
                   tblARI <- tblARI[-dim(tblARI)[1],]
                   
                   # initialize the TDP map
                   img_tdps <- array(0, fileInfo$header$dim[2:4])
                   # update TDP map
                   for (i in clus_labels) {
                     img_tdps[img_clus==i] <- tblARI[length(clus_labels)-i+1, 4]
                   }
                   
                   if (length(clus_labels) == 1) { # one cluster only
                     
                     Vox_xyzs <- as.integer(tblARI[5:7])
                     xyzV     <- paste0("(",
                                        Vox_xyzs[1], ", ", 
                                        Vox_xyzs[2], ", ",
                                        Vox_xyzs[3], 
                                        ")")
                     MNI_xyzs <- as.integer(xyz2MNI(Vox_xyzs, fileInfo$header))
                     xyzM     <- paste0("(",  
                                        MNI_xyzs[1], ", ",
                                        MNI_xyzs[2], ", ",
                                        MNI_xyzs[3],
                                        ")")
                     
                     tblARI <- data.frame(clus = clus_labels[1],
                                          size = as.integer(tblARI[1]),
                                          tdps = tblARI[4],
                                          maxT = tblARI[8],
                                          xyzV = xyzV, 
                                          xyzM = xyzM)
                     
                   } else { # found more than one clusters
                     
                     Vox_xyzs <- matrix(as.integer(tblARI[,5:7]), length(clus_labels), 3)
                     xyzV     <- paste0("(",
                                        as.integer(Vox_xyzs[,1]), ", ",
                                        as.integer(Vox_xyzs[,2]), ", ",
                                        as.integer(Vox_xyzs[,3]), 
                                        ")")
                     MNI_xyzs <- xyz2MNI(Vox_xyzs, fileInfo$header)
                     xyzM     <- paste0("(",
                                        as.integer(MNI_xyzs[,1]), ", ",
                                        as.integer(MNI_xyzs[,2]), ", ",
                                        as.integer(MNI_xyzs[,3]), 
                                        ")")
                     
                     tblARI <- data.frame(clus = clus_labels,
                                          size = as.integer(tblARI[,1]),
                                          tdps = tblARI[,4], 
                                          maxT = tblARI[,8],
                                          xyzV = xyzV, 
                                          xyzM = xyzM)
                   }
                   
                   # update cluster & TDP images
                   img_clus[img_clus == 0] <- NA
                   img_tdps[img_clus == 0] <- NA
                   xyz$img_clus            <- img_clus
                   xyz$img_tdps            <- img_tdps
                   
                 })
    
    rownames(tblARI) <- paste0("cl", clus_labels)
    colnames(tblARI) <- c("Cluster",
                          "Size",
                          "TDP",
                          "max(Z)",
                          "Voxel position<br/>(x, y, z)",
                          "MNI coordinates<br/>(x, y, z)")
    
    # update fileInfo
    fileInfo$tblARI   <<- tblARI
    fileInfo$tblXYZ   <<- Vox_xyzs
    fileInfo$img_clus <<- img_clus
    fileInfo$img_tdps <<- img_tdps
    
  })
  
  # observe event after pressing button to form supra-threshold clusters 
  # using TDP threshold
  observeEvent(input$TDPclusButton, {
    
    withProgress(message = "Calculation in progress.", value = 0,
                 detail  = "This may take a few seconds...", {
                   
                   # find all maximal STCs with a TDP threshold
                   clusterlist <- ARIbrain::answerQuery(input$tdpthres, stcs, reslist$SIZE, marks, tdps, reslist$CHILD)
                   
                   # sort clusters by descending cluster size
                   n <- length(clusterlist)
                   if (n == 0) { # return warning if there are no clusters found
                     
                     showModal(
                       modalDialog(
                         title = "No clusters",
                         "No clusters can be formed with the given TDP threshold.
                          Please reduce the threshold."
                       )
                     )
                     return(NULL)
                     
                   } else if (n > 1) {
                     # sort clusters in terms of size (descending order)
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
                   
                   tblARI <- plyr::laply(1:n, function(i) {
                     if (fileInfo$type == "p") {
                       clus_stat <- -qnorm(fileInfo$data[indexp][clusterlist[[i]]+1])
                     } else {
                       clus_stat <- fileInfo$data[indexp][clusterlist[[i]]+1]
                     }
                     id_clus   <- which.max(clus_stat)
                     id_max    <- clusterlist[[i]][id_clus]+1
                     xyz_max   <- ARIbrain:::ids2xyz(as.integer(indexp[id_max]-1), fileInfo$header$dim[2:4])
                     
                     clus_size <- length(clusterlist[[i]])
                     clus_tdp  <- tdps[clusterlist[[i]][clus_size]+1]
                     unlist(c(Size       = clus_size, 
                              FalseNull  = round(clus_size*clus_tdp), 
                              TrueNull   = round(clus_size*(1-clus_tdp)), 
                              ActiveProp = clus_tdp,
                              x_max      = xyz_max[,1],
                              y_max      = xyz_max[,2],
                              z_max      = xyz_max[,3],
                              maxZ       = clus_stat[id_clus]))
                   })
                   if (is.null(dim(tblARI))) tblARI <- t(as.matrix(tblARI))
                   
                   # update result table
                   if (n == 1) { # one cluster only
                     
                     Vox_xyzs <- tblARI[5:7]
                     xyzV     <- paste0("(",
                                        as.integer(Vox_xyzs[1]), ", ", 
                                        as.integer(Vox_xyzs[2]), ", ",
                                        as.integer(Vox_xyzs[3]), 
                                        ")")
                     MNI_xyzs <- xyz2MNI(Vox_xyzs, fileInfo$header)
                     xyzM     <- paste0("(",
                                        as.integer(MNI_xyzs[1]), ", ",
                                        as.integer(MNI_xyzs[2]), ", ",
                                        as.integer(MNI_xyzs[3]),
                                        ")")
                     
                     tblARI <- data.frame(clus = 1,
                                          size = as.integer(tblARI[1]),
                                          tdps = tblARI[4],
                                          maxT = tblARI[8],
                                          xyzV = xyzV, 
                                          xyzM = xyzM)
                     
                   } else { # found more than one clusters
                     
                     Vox_xyzs <- matrix(as.integer(tblARI[,5:7]), n, 3)
                     xyzV     <- paste0("(",
                                        as.integer(Vox_xyzs[,1]), ", ",
                                        as.integer(Vox_xyzs[,2]), ", ",
                                        as.integer(Vox_xyzs[,3]), 
                                        ")")
                     MNI_xyzs <- xyz2MNI(Vox_xyzs, fileInfo$header)
                     xyzM     <- paste0("(",
                                        as.integer(MNI_xyzs[,1]), ", ",
                                        as.integer(MNI_xyzs[,2]), ", ",
                                        as.integer(MNI_xyzs[,3]), 
                                        ")")
                     
                     tblARI <- data.frame(clus = n:1,
                                          size = as.integer(tblARI[,1]),
                                          tdps = tblARI[,4], 
                                          maxT = tblARI[,8],
                                          xyzV = xyzV, 
                                          xyzM = xyzM)
                     
                   }
                   
                 })
    
    rownames(tblARI) <- paste0("cl", n:1)
    colnames(tblARI) <- c("Cluster",
                          "Size",
                          "TDP",
                          "max(Z)",
                          "Voxel position<br/>(x, y, z)",
                          "MNI coordinates<br/>(x, y, z)")
    
    # initialize the cluster & TDP maps
    img_clus <- array(0, fileInfo$header$dim[2:4])
    img_tdps <- array(0, fileInfo$header$dim[2:4])
    # update cluster & TDP maps
    for (i in 1:n) {
      img_clus[indexp[clusterlist[[i]]+1]] <- n-i+1
      img_tdps[indexp[clusterlist[[i]]+1]] <- tblARI[i, 4]
    }
    
    # update cluster & TDP images
    img_clus[img_clus == 0] <- NA
    img_tdps[img_clus == 0] <- NA
    xyz$img_clus            <- img_clus
    xyz$img_tdps            <- img_tdps
    
    # update fileInfo
    fileInfo$tblARI   <<- tblARI
    fileInfo$tblXYZ   <<- Vox_xyzs
    fileInfo$img_clus <<- img_clus
    fileInfo$img_tdps <<- img_tdps
  })
  
  # render sagittal, coronal & axial result boxes (UI)
  output$sagResultBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("x_slider", label = NULL, step = 1, value = xyz$x,
                  min = 1, max = fileInfo$header$dim[2]),
      plotOutput("mapSag", click = "click_sag")
    )
  })
  output$corResultBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("y_slider", label = NULL, step = 1, value = xyz$y,
                  min = 1, max = fileInfo$header$dim[3]),
      plotOutput("mapCor", click = "click_cor")
    )
  })
  output$axiResultBox <- renderUI({
    box(
      width = 4,
      background = "black",
      sliderInput("z_slider", label = NULL, step = 1, value = xyz$z,
                  min = 1, max = fileInfo$header$dim[4]),
      plotOutput("mapAxi", click = "click_axi")
    )
  })
  
  # observe event after user clicking the image
  observeEvent(input$click_sag, {
    xyz$y <- round(input$click_sag$x)
    xyz$z <- round(input$click_sag$y)
  })
  observeEvent(input$click_cor, {
    xyz$x <- round(input$click_cor$x)
    xyz$z <- round(input$click_cor$y)
  })
  observeEvent(input$click_axi, {
    xyz$x <- round(input$click_axi$x)
    xyz$y <- round(input$click_axi$y)
  })
  
  # observe event after user changing the sliders
  observeEvent(input$x_slider, {
    xyz$x <- input$x_slider
  })
  observeEvent(input$y_slider, {
    xyz$y <- input$y_slider
  })
  observeEvent(input$z_slider, {
    xyz$z <- input$z_slider
  })
  
  # render gradient/cluster maps
  output$mapSag <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64,0,1),
              FALSE, zlim = c(0,1), views = c("sag"))
    if (!is.null(xyz$img_clus)) {
      plotImage(xyz$img_clus, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, 
                rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, 
                zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), 
                views = c("sag"))
    }
    abline(h = xyz$z, v = xyz$y, col = "green")
  })
  output$mapCor <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64,0,1), 
              FALSE, zlim = c(0,1), views = c("cor"))
    if (!is.null(xyz$img_clus)) {
      plotImage(xyz$img_clus, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, 
                rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, 
                zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])),
                views = c("cor"))
    }
    abline(h = xyz$z, v = xyz$x, col = "green")
  })
  output$mapAxi <- renderPlot({
    plotImage(fileInfo$grad_map, fileInfo$header$dim[2:4],
              xyz$x, xyz$y, xyz$z, gray.colors(64,0,1), 
              FALSE, zlim = c(0,1), views = c("axi"))
    if (!is.null(xyz$img_clus)) {
      plotImage(xyz$img_clus, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, 
                rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, 
                zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), 
                views = c("axi"))
    }
    abline(h = xyz$y, v = xyz$x, col = "green")
  })
  
  # Download images
  output$dlButton <- downloadHandler(
    filename = "gradmap.nii",
    # function() {
    #   paste0(fileInfo$outputDir, .Platform$file.sep, "gradmap.nii")
    # },
    content = function(file) {
      RNifti::writeNifti(fileInfo$grad_map, file, template = fileInfo$data)
    }
  )
  
}

makeMenu4 <- function(input, output, session) {
  
  # observe event after pressing the button
  observeEvent(input$runButton, {
    observeMenu4(input, output, session)
  })
  
}
