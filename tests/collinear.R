library(Amelia)
data(africa)

africa$pop2 <- africa$population

a.out <- try(amelia(africa, ts = 1, cs = 2))
if (a.out != "Error in amcheck(x = x, m = m, idvars = numopts$idvars, priors = priors,  : \n  The variable  pop2 is perfectly collinear with another variable in the data.\n\n") {
  stop("collinearity check fails")
}
