library(Amelia)

data(freetrade)

bds <- matrix(c(3, 30, 32), nrow = 1, ncol = 3)

set.seed(12345)
a.out.bds <- amelia(freetrade, ts = "year", cs = "country", bounds = bds,
                    max.resample = 10, p2s = 0)

out <- range(a.out.bds$imputations$imp1[is.na(freetrade[,3]),3])



if (out[1] < 30) {
  stop("lower bounds not working")
}

if (out[2] > 32) {
  stop("upper bounds not working")
}
