# ARIbrain
All-Resolution Inference (ARI) for brain imaging

# Introduction
`ARIbrain` is the R-package for All-Resolution Inference (ARI) in neuroscience. It allows researchers to estimate the True Discovery Proportion (TDP) of any cluster in a statistical map derived from a (functional) MRI experiment. Statistical maps can be derived using your favorite fMRI analysis package (e.g. SPM, FSL, AFNI). It is convenient for the output to be in NIfti format, as this can be read in by the package. Alternatively you could use an R array as input as well (as the nifti files will be converted to an array internally). 

ARIbrain can be used in two different 'modes'. Using `ARI`, we show how to compute lower bound for proportion of active voxels (or any other spatially located units) within given clusters. Alternatively, with `ARIcluster`, we show how to find maximal clusters under the given threshold of true discovery proportion (TDP).

ARIbrain requires R to run and the 'ARIbrain'-package to be installed. For non R-users the easiest way to install R is in combination with Rstudio. You can find the instructions how to install R and RStudio.

## Installing the 'ARIbrain' package. 

### Step 1, Installing R and Rstudio.
Go [here](https://posit.co/download/rstudio-desktop/) to download R and Rstudio from the Posit website (you need the RStudio Desktop FREE version). First install R and then install RStudio.

### Step 2, Installing ARIbrain
You can install the stable version of ARIbrain from [CRAN](https://cran.r-project.org/web/packages/ARIbrain/), or use the *Tools > Install packages* option from Rstudio (select CRAN Repository and search for ARIbrain, leave the install dependencies option checked), or use the `install.packages('ARIbrain')` command in R/Rstudio. The development version of ARIbrain can be downloaded from the ARIbrain GitHub repository using the 'devtools' package. First install this package using `install.packages('devtools')`, and then install ARIbrain using the following command: `devtools::install_github('wdweeda/ARIbrain')`.

## Standard ARIbrain
Download the files from the arribrain_appdir and in Rstudio run: `setwd('~/aribrain_appdir')`, where '~/aribrain_appdir' is the directory you just downloaded these files to. Also download the simes_ari.R in the same directory. Run source('simes_ari.R') and enjoy the results.

## ARIbrain interactive exploration app (beta)
In addition to the ARIbrain package, we have an ARIbrain-app that can be used, in addition to the normal ARI analysis to interactively explore all clusters (for example, you can apply a different threshold for each cluster separately).

To do this download the files in the 'aribrain_app' directory and in Rstudio run: `setwd('~/aribrain_appdir')`, where '~/aribrain_appdir' is the directory you just downloaded these files to. Then run: `shiny::runApp('~/aribrain_appdir/aribrain_app')`. You will now get in interactive mode.

## FSL command-line add-on
You can also append your FSL cluster analysis with TDPs. For this go the cope directory of interest in your multilevel gfeat analysis in the terminal using cd 'gfeatdir/copedir/', where 'gfeatdir/copedir/' is the cope directory of your multilevel analysis. Also download the get_tdp.R file (for example in the 'download' directory. The command line has the following input

```
Rscript get_tdp.R --zstat=<filename> --cluster=<filename> [options]

Compulsory arguments:
--zstat/tstat   filename of z-statistics file (nifti)
--cluster       filename of cluster-index file (nifti)
--df            if tstat is specified, df is also needed

Optional arguments:
[method]
--alpha         nominal alpha level of TDPs (two-sided), default is 0.05
[output]
--outfile       optional output file of TDP values (nifti)
--outtable      optional output table of TDP values (txt)
--intable       optional input table of clusters (TDPs will be added)
--inhtml        optional input html cluster file (TDPs will be added)
[options]
--verbose       print additional information on TDPs
--quiet         suppresses almost all output to console
--help          prints this message
```

The following command will append your cluster.html file and save a text file with TDPs and a nifti file with voxels having the TDP value of the cluster they belong to. In the cope directory run: `Rscript /download/get_tdp.R --zstat=./stats/zstat1.nii.gz --cluster=cluster_mask_zstat1.nii.gz --alpha=0.05 --outtable=tdptable.txt --outfile=tdpclus.nii.gz --inhtml=cluster_zstat1_std.html`
