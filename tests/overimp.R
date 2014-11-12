library(Amelia)
data(africa)
af2 <- na.omit(africa)

oi <- matrix(c(1:10,rep(4, 10)), nrow = 10)

## error here
a.out <- amelia(af2, cs = 2, overimp = oi)

if (a.out$imputations[[1]][1,4] == a.out$imputations[[2]][1,4]) {
  stop("overimp is broken")
}
