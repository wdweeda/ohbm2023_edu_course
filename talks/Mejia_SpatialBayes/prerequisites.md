
### Pre-requisites for spatial Bayesian modeling

Please complete the following prerequisites for spatial Bayesian modeling with the BayesfMRI R package.  

Note: Any package available from CRAN can be installed with `install.packages("[package name here]")`, while packages available from Github need to be installed using `install_github("[repo_name/package_name]")`. The `install_github()` function is from the `devtools` package, which is available from CRAN.  To use any installed packages, they must first be loaded at the beginning of each R session or script using `library([packagename])`.

1. Install the latest version of R (4.3.1) and RStudio.  These must be updated to avoid errors.

2. Install the latest testing version of R-INLA for Bayesian computation with INLA by running `install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=FALSE)` and the dependencies `sp` and `foreach` from CRAN.  Load each package in R to check it was installed without errors.

3. Download the Connectome Workbench and install the `ciftiTools` R package from CRAN.  Load the package in R and follow the instructions to set the path to the Connectome Workbench.

4. Install and load the `BayesfMRI` package from CRAN and load it in R.

5. Install and load a few additional packages we'll use during the demo from CRAN: manipulateWidget, ggplot2, ggthemes and reshape2


