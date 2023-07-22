# ARIbrain-app
All-Resolutions Inference (ARI) for brain imaging - Shiny application

## Preparations

### (1) Install R and Rstudio.
Please visit [Rstudio](https://posit.co/download/rstudio-desktop/) to download R and Rstudio from the Posit website (you need the RStudio Desktop FREE version). Both R and Rstudio need to be installed.

### (2) Install necessary R packages.
The current version of ARIbrain app can directly detect whether or not the required R packages have been installed. If not, it will first download & install those packages. It might take few minutes, so please wait a bit...

## Run ARIbrain-app

### (1) The latest version of ARIbrain-app can be obtained at [ARIbrain-app](https://github.com/wdweeda/ARIbrain-app/) or downloaded using 
``` r
git clone https://github.com/wdweeda/ARIbrain-app.git
```
The repository includes the app and the example data files including the z-score image ```z.nii.gz``` and brain mask ```mask.nii.gz```. 

### (2) Aftering getting the app, please first launch Rstudio, and set the Rstudio working directory to where the app is saved with
``` r
setwd("/path/to/ARIbrain-app")
```

### (3) Run ARIbrain-app locally in Rstudio in two ways:

#### 1. Open ```app.R``` in Rstudio, and press the ```Run App``` button on the top right corner.

#### 2. Copy and run the code in ```app.R``` on Rstudio console.

# Find bugs?
The ARIbrain app is currently on available as beta-version that can only be run locally. We will kepp improving and updating the app. You can also use the [ARIbrain](https://github.com/wdweeda/ARIbrain) R package to do ARI. If you want more information or find any bugs, please contact the maintainer (w dot d dot weeda at fsw dot leidenuniv dot nl).
