# Amelia II

[Amelia II][] is an R package for the multiple imputation of multivariate incomplete data. It uses an algorithm that combines bootstrapping and the EM algorithm to take draws from the posterior of the missing data. The Amelia package includes normalizing transformations, cell-level priors, and methods for handling time-series cross-sectional data. 

## How to install

### Installation requirements
`Amelia` requires [R][] version 2.14.0 or higher. 

### Manual installation
```R
install.packages("Amelia", repos = "http://r.iq.harvard.edu/",type  = "source")
```

[Amelia II]: http://gking.harvard.edu/amelia
[R]: http://cran.r-project.org
