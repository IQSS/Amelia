# Amelia II


 <!-- badges: start -->
  [![R build status](https://github.com/IQSS/Amelia/workflows/R-CMD-check/badge.svg)](https://github.com/IQSS/Amelia/actions)
  [![CRAN
version](http://www.r-pkg.org/badges/version/Amelia)](https://cran.r-project.org/package=Amelia)

  <!-- badges: end -->


## Overview

[Amelia II][] is an R package for the multiple imputation of multivariate incomplete data. It uses an algorithm that combines bootstrapping and the EM algorithm to take draws from the posterior of the missing data. The Amelia package includes normalizing transformations, cell-level priors, and methods for handling time-series cross-sectional data. 


## How to install

To install the latest version of Amelia, which requires [R][] version 2.14.0 or higher, simply use the standard R installation tools:

```r
install.packages("Amelia")
```

If you would to use the current development release of Amelia (which may be unstable), run the following: 

```r
require(devtools)
devtools::install_github("IQSS/Amelia")
```


## Getting started with Amelia

The main function in the Amelia package is `amelia()` which will perform multiple imputation on a data frame. It allows for easy setting of time-series and unit variables via the `ts` and `cs` arguments. 

```r
library(Amelia)
data(africa)

a.out <- amelia(africa, m = 5, ts = "year", cs = "country")
```

## AmeliaView GUI

Once installed, you can access most of the Amelia functionality through an interactive GUI by running the following command:

```r
Amelia::AmeliaView()
```



[Amelia II]: http://gking.harvard.edu/amelia
[R]: https://cran.r-project.org
