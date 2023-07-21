ab_loaded <- require(ARIbrain,quietly = T)
rnifti_loaded <- require(RNifti, quietly = T)

cat('TDP for clusters (c) 2023\n')

#check for package loads
if(!(ab_loaded & rnifti_loaded)) {
  
  cat('One or more packages not loaded, trying to install packages...\n')
  
  install.packages(c('RNifti','ARIbrain'),repos = 'https://cloud.r-project.org/')
  
  inst <- installed.packages()
  
  if(!(length(grep('ARIbrain',inst[,1]))>0 & length(grep('RNifti',inst[,1]))>0)) {
        stop('Failed to install packages, use the following command in R: install.packages(c(\'RNifti\',\'ARIbrain\')\n')
  }
  
}

#define valid arguments
required_args <- c('zstat','cluster')
valid_args <- c('alpha','outfile','outtable','help','verbose')

#get arguments
args <- tolower(commandArgs(trailingOnly = T))

#[remove]
#print(args)
valid <- TRUE

#help requested?
if(length(grep('--help',tolower(args)))>0) {
  args <- character(0)
  valid <- FALSE 
}

#check valid args (too few or too many)
if(length(args)>0) {
  if(!( (length(grep('--zstat',args)) == 1 | length(grep('--tstat',args))==1 ) & length(grep('--cluster',args)) == 1) ) {
    cat('Compulsory arguments missing or multiple equal arguments found\n')
    args <- character(0)
    valid <- FALSE
  }
  
  if(length(grep('--tstat',args))==1 & length(grep('--df',args) == 0)) {
    valid <- FALSE
    cat('df for tstat missing\n')
  }
  
  if((length(grep('--zstat',args)) == 1 | length(grep('--tstat',args))==1)) {
    valid <- FALSE
    cat('both tstat and zstat are given as arguments, please give only one\n')
  }
  
  
}


#print usage to console  
if(length(args)==0) {
  cat('\n')
  cat('Usage:\n')
  cat('Rscript get_tdp.R --zstat=<filename> --cluster=<filename> [options]\n')
  cat('\n')
  cat('Compulsory arguments:\n')
  cat('--zstat/tstat   filename of z-statistics file (nifti)\n')
  cat('--cluster       filename of cluster-index file (nifti)\n')
  cat('--df            if tstat is specified, df is also needed\n')
  cat('\n')
  cat('Optional arguments:\n')
  cat('[method]\n')
  cat('--alpha         nominal alpha level of TDPs (two-sided), default is 0.05\n')
  cat('[output]\n')
  cat('--outfile       optional output file of TDP values (nifti)\n')
  cat('--outtable      optional output table of TDP values (txt)\n')
  cat('--intable       optional input table of clusters (TDPs will be added)\n')
  cat('--inhtml        optional input html cluster file (TDPs will be added)\n')
  cat('[options]\n')
  cat('--verbose       print additional information on TDPs\n')
  cat('--quiet         suppresses almost all output to console\n')
  cat('--help          prints this message\n')
  cat('\n')
  
}

if(length(grep('--quiet',args))!=0) quiet <- TRUE else quiet <- FALSE

#check if we are good to go
if(valid) {
  
  #check if alpha is given, else append
  if(length(grep('--alpha',args))==0) args <- c(args, '--alpha=0.05')
  
  ari_alpha <- strsplit(args[grep('--alpha',args)],'\\=')[[1]][2]
  if(is.na(as.numeric(ari_alpha))) stop(paste0('Invalid alpha value (',ari_alpha,')')) 
  ari_alpha <- as.numeric(ari_alpha)
  
  if(!quiet) cat(paste0('Calculated assuming Simes\' inequality with ',round((1-ari_alpha)*100,1),'% confidence.\n\n'))
  
  #check what data to be red in
  
  
  #read in z-stats file
  if(length(grep('--zstat',args)) == 1) {
    zfile <- strsplit(args[grep('--zstat',args)],'\\=')[[1]][2]
    znifti <- try(RNifti::readNifti(zfile))
    if(class(znifti)[1]=='try-error') stop('Z-statistics file not found\n')
    zstat <- znifti[]
    statdata <- zstat
    
    #make p-values
    if(!quiet) cat(' > converted z-stats to 2-sided p-values\n')
    pvalues2 <- (1-pnorm(abs(zstat)))*2
    #print(summary(pvalues2))
    
    #print(summary(zstat))
  }
  
  if(length(grep('--tstat',args)) == 1) {
    tfile <- strsplit(args[grep('--tstat',args)],'\\=')[[1]][2]
    tnifti <- try(RNifti::readNifti(tfile))
    if(class(znifti)[1]=='try-error') stop('Z-statistics file not found\n')
    tstat <- tnifti[]
    statdata <- tstat
    
    #make p-values
    if(!quiet) cat(' > converted z-stats to 2-sided p-values\n')
    pvalues2 <- (1-pt(abs(tstat),df = as.numeric(trsplit(args[grep('--df',args)],'\\=')[[1]][2])))*2
    #print(summary(pvalues2))
    
    #print(summary(tstat))
  }
  
  
  
  
  #read in cluster file
  cfile <- strsplit(args[grep('--cluster',args)],'\\=')[[1]][2]
  cnifti <- try(RNifti::readNifti(cfile))
  
  if(class(cnifti)[1]=='try-error') stop('Cluster mask file not found\n')
  cstat <- cnifti[]
  
 
  
  #make mask 
  mask <- zstat!=0
  
  #do ARIBRAIN
  #sink output if verbose is of
  if(length(grep('--verbose',args))==0) {
    sink(file=nullfile(), type='output')  
  } 
  
  #call ARIBbrain
  ari_out <- ARIbrain::ARI(Pmap = pvalues2, clusters = cstat, mask = mask, alpha = ari_alpha, Statmap = zstat)
  
  #unsink output (suppress warning when verbose was off)
  suppressWarnings(sink())
  
  #write tdptable when requested
  if(length(grep('--outtable',args))!=0) {
    
    if(!quiet) cat(' > writing TDP table\n')
    txtfile <- strsplit(args[grep('--outtable',args)],split = '\\=')[[1]][2]
    write.table(ari_out, file = txtfile, row.names = F, col.names = T, quote = F)  
    
  }
  
  #write clusteroutput
  if(length(grep('--outfile',args))!=0) {
    
    if(!quiet) cat(' > writing cluster nifti with TDP values\n')
    
    #replace cluster with TDP values
    tdpstat <- cstat
    maxclus <- max(cstat)
    
    #[do error checking here on size of TDP output table]
    
    for(curclus in 1:maxclus) {
      tdpstat[which(cstat==curclus,arr.ind=T)] <- ari_out[which(rownames(ari_out)==paste0('cl',curclus)),4]
    }
    
    #write file
    tdpfile <- strsplit(args[grep('--outfile',args)],split = '\\=')[[1]][2]
    RNifti::writeNifti(image = tdpstat, file = tdpfile , template = cnifti)  
    
  }
  
  #write tdptable when requested
  if(length(grep('--intable',args))!=0) {
    
    if(!quiet) cat(' > adding TDPs to cluster table file\n')
    infile <- strsplit(args[grep('--intable',args)],split = '\\=')[[1]][2]
    intable <- read.table(file = infile, sep='\t', header = T)
    
    addmat <- data.frame()
    for(i in 1:nrow(intable)) {
      addmat <- rbind(addmat,data.frame(TDP=ari_out[grep(as.character(intable[i,1]),rownames(ari_out)),4]))
    }
    
    #save table to text
    outtable <- cbind(intable[1:4],addmat,intable[5:ncol(intable)])
    inoutfile <- paste0(strsplit(infile, split = '\\.')[[1]][1], '_tdp.',strsplit(infile, split = '\\.')[[1]][2])
    write.table(outtable, file = inoutfile, quote = F, row.names = F, col.names = T, sep='\t')
    
  }
  
  #write tdptable to html
  if(length(grep('--inhtml',args))!=0) {
    
    if(!quiet) cat(' > adding TDPs to cluster html\n')
    infile_html <- strsplit(args[grep('--inhtml',args)],split = '\\=')[[1]][2]
    intable_html <- as.matrix(read.table(file = infile_html, sep='\t', header = F, stringsAsFactors = F))
    
    #add TDP's after log10 P-value
    cindex <- grep('Cluster Index', intable_html)[1]
    cend <- grep('</TABLE>',intable_html)[1]
    
    for(i in (cindex+1):(cend-1)) {
        
      curdata <- strsplit(intable_html[i,],'<td>')[[1]]
      curclus <- curdata[2]
      tdprow <- ari_out[which(rownames(ari_out)==paste0('cl',curclus)),]
    
      #make append rows
      repl <- paste0(curdata[1],'<td>',paste0(curdata[2:5], collapse = '<td>'),paste0('<td>',round(tdprow[4],4),'<td>'),paste0(curdata[6:17], collapse = '<td>') )
      
      #append row in data
      intable_html[i,] <- repl
      
    }
    
    #append cindex row with rowname TDP
    namerow <- strsplit(intable_html[cindex,],'<th>')[[1]]
    replname <- paste0(namerow[1],'<th>',paste0(namerow[2:5], collapse = '<th>'),paste0('<th>','TDP','<th>'),paste0(namerow[6:17], collapse = '<th>') )
    intable_html[cindex,] <- replname
    
    #add information to html topline.
    inforow <- grep('to main FEAT report',intable_html)
    infodata <- intable_html[inforow,]
    inforepl <- paste0(strsplit(infodata,'<hr>')[[1]],paste0('<br>','True Discovery Proportions (TDP) calculated assuming Simes inequality with ',round((1-ari_alpha)*100,1),'% confidence (alpha = ',ari_alpha,'). Z-values were transformed to 2-sided p-values. Calculated using the ARIBrain packge in R.'),'<hr>')
    intable_html[inforow,] <- inforepl
    
    #save html file
    outhtml <- paste0(strsplit(infile_html, split = '\\.')[[1]][1], '_tdp.',strsplit(infile_html, split = '\\.')[[1]][2])
    write.table(intable_html, file = outhtml, quote =FALSE, sep='\n',col.names = F, row.names = F)
    
  }
  
  
  if(!quiet) {
    cat(paste0('\n<TDP output>\n'))
    show(ari_out)  
    cat('\n')
  }
  
  
}


