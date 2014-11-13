library(Amelia)

x <- rnorm(100)
s <- x + rnorm(100, 0, 0.1)
vv <- rep(0.1^2/var(s), 100)

df <- data.frame(x, s)
mop <- moPrep(df,s ~ s,error.proportion = vv)
a.out <- amelia(mop)
