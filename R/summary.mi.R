summary.mi <- function(object, ...) {
  m <- length(object)
  nv <- length(object[[1]])
  nr <- nrow(object[[1]])
  cat("[", m, "imputations,", nv, "variables,", nr, "rows]\n\n")
  summary(do.call(rbind, object))
}
