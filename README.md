# Amelia II

[Amelia II][] is an R package for the multiple imputation of multivariate incomplete data. It uses an algorithm that combines bootstrapping and the EM algorithm to take draws from the posterior of the missing data. The Amelia package includes normalizing transformations, cell-level priors, and methods for handling time-series cross-sectional data. 

## How to install

### Installation requirements
`Amelia` requires [R][] version 2.14.0 or higher. 

### Manual installation
```R
install.packages("Amelia")
```

### Installing unstable developer version:
```R
require(devtools)
install_github("IQSS/Amelia", ref = "develop")
```

[Amelia II]: http://gking.harvard.edu/amelia
[R]: https://cran.r-project.org
