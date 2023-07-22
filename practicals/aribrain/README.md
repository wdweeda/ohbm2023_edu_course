# ARIbrain
All-Resolution Inference (ARI) for brain imaging

# Introduction
`ARIbrain` is the R-package for All-Resolution Inference (ARI) in neuroscience. It allows researchers to estimate the True Discovery Proportion (TDP) of any cluster in a statistical map derived from a (functional) MRI experiment. Statistical maps can be derived using your favorite fMRI analysis package (e.g. SPM, FSL, AFNI). It is convenient for the output to be in NIfti format, as this can be read in by the package. Alternatively you could use an R array as input as well (as the nifti files will be converted to an array internally). 

ARIbrain can be used in two different 'modes'. Using `ARI`, we show how to compute lower bound for proportion of active voxels (or any other spatially located units) within given clusters. Alternatively, with `ARIcluster`, we show how to find maximal clusters under the given threshold of true discovery proportion (TDP).

ARIbrain requires R to run and the 'ARIbrain'-package to be installed. For non R-users the easiest way to install R is in combination with Rstudio. You can find the instructions how to install R and RStudio.

## Installing the 'ARIbrain' package and command-line script. 

### Step 1, Installing R and Rstudio.
Go [here](https://posit.co/download/rstudio-desktop/) to download R and Rstudio from the Posit website (you need the RStudio Desktop FREE version). First install R and then install RStudio.

### Step 2, Installing ARIbrain
You can install the stable version of ARIbrain from [CRAN](https://cran.r-project.org/web/packages/ARIbrain/), or use the *Tools > Install packages* option from Rstudio (select CRAN Repository and search for ARIbrain, leave the install dependencies option checked), or use the `install.packages('ARIbrain')` command in R/Rstudio. The development version of ARIbrain can be downloaded from this GitHub repository using the 'devtools' package. First install this package using `install.packages('devtools')`, and then install ARIbrain using the following command: `devtools::install_github('wdweeda/ARIbrain')`.

## ARIbrain interactive exporation app


