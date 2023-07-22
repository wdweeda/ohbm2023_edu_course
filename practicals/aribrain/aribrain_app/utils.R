# ARIbrain utility functions

# check files and return status/fields
checkFileType <- function(inFile) {
  
  # initialise file-type properties
  fileTypeInfo <- list(type = "u",
                       df = 0,
                       twosided = TRUE,
                       valid = FALSE,
                       selected = "unknown",
                       filename = inFile$name,
                       header = NULL,
                       data = NULL,
                       mask = NULL)
  class(fileTypeInfo) <- "aribrain-filetype"
  
  file.rename(
    inFile$datapath, 
    paste0(dirname(inFile$datapath), .Platform$file.sep, inFile$name)
  )
  
  data <- try(
    suppressWarnings(
      RNifti::readNifti(
        paste0(dirname(inFile$datapath), .Platform$file.sep, inFile$name))
    ), silent = TRUE
  )
  
  if(class(data)[1] != "try-error") {
    
    # set type to valid
    fileTypeInfo$valid <- TRUE
    
    # read in header to determine statistic type
    header <- RNifti::niftiHeader(data)
    fileTypeInfo$header <- header
    
    # set data to output list
    fileTypeInfo$data <- data
    fileTypeInfo$mask <- (!is.na(data)) & (data!=0)
    
    # set type based on intent_code
    if(header$intent_code != 0) {
      
      if(header$intent_code == 3)  fileTypeInfo$type <- "t"
      if(header$intent_code == 5)  fileTypeInfo$type <- "z"
      if(header$intent_code == 22) fileTypeInfo$type <- "p"
      
      cat("determined intent_code", fileTypeInfo$type, "\n")
      
    } else {
      
      # determine type on descrip
      descrip = header$descrip
      cat(descrip, "\n")
      
      if(length(grep("SPM\\{T", descrip)) > 0) fileTypeInfo$type <- "t"
      cat("determined descrip", fileTypeInfo$type, "\n")
      
      df = try(
        as.numeric(
          strsplit(strsplit(descrip, "\\[")[[1]], "\\]")[[2]][1]
        ), silent = TRUE
      )
      
      if(class(df) != "try-error") {
        fileTypeInfo$df <- df
        cat("determined df", fileTypeInfo$df, "\n")
      }
    }
    
    # set selected for dropdown menu
    if(fileTypeInfo$type == "t") fileTypeInfo$selected <- "t-map"
    if(fileTypeInfo$type == "z") fileTypeInfo$selected <- "z-map"
    if(fileTypeInfo$type == "p") fileTypeInfo$selected <- "p-map"
    if(fileTypeInfo$type == "u") fileTypeInfo$selected <- "unknown"
    
  } else {
    # NO VALID NIFTI (will return an invalid state to fileTypeInfo)
  }
  
  return(fileTypeInfo)
}


plotImage <- function(data, dims, x, y, z, colrng, overlay, zlim, 
                      views = c("sag", "cor", "axi")) {
  
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  
  # sagittal view
  if (any(views == "sag")) {
    image(1:dims[2], 1:dims[3], data[x,,], 
          col = colrng, add = overlay, zlim = zlim, 
          axes = FALSE, xlab = "", ylab = "")
  }
  # coronal view
  if (any(views == "cor")) {
    image(1:dims[1], 1:dims[3], data[,y,], 
          col = colrng, add = overlay, zlim = zlim,
          axes = FALSE, xlab = "", ylab = "")
  }
  # axial view
  if (any(views == "axi")) {
    image(1:dims[1], 1:dims[2], data[,,z], 
          col = colrng, add = overlay, zlim = zlim,
          axes = FALSE, xlab = "", ylab = "")
  }
  
}


# convert voxel location to MNI coordinates
# xyz - (nrow x 3) matrix
xyz2MNI <- function(xyz, hdr) {
  
  # transformation matrix
  transMatrix <- rbind(hdr$srow_x, hdr$srow_y, hdr$srow_z)
  
  if(is.null(dim(xyz))) {
    MNI_xyz <- transMatrix %*% c(xyz-1, 1)
  }
  else {
    MNI_xyz <- t(transMatrix %*% t(cbind(xyz-1, rep(1, nrow(xyz)))))
  }
  
  return(MNI_xyz)
}

# convert coordinates to index
xyz2index <- function(x, y, z, dims) {
  return( (z-1)*dims[1]*dims[2] + (y-1)*dims[1] + (x-1) )
} 

