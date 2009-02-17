
summary.amelia <- function(object, ...) {

  percent.missing <- colMeans(object$missMatrix)
  n.patterns <- nrow(unique(object$missMatrix))

  rows.imputed <- nrow(na.omit(object$imputations[[1]]))
  rows.lwd <- sum(rowSums(object$missMatrix)==0)

  print.amelia(object)

  cat("Rows after Listwise Deletion: ",rows.lwd,"\n")
  cat("Rows after Imputation: ", rows.imputed,"\n")
  cat("Patterns of missingness in the data: ", n.patterns, "\n\n")

  cat("Percent Missing for individual variables: \n")
  cat("-----------------------------------------\n\n")
  tb <- data.frame(cbind(percent.missing))
  rownames(tb) <- colnames(object$imputations[[1]])
  colnames(tb) <- "Percent Missing"
  print(tb)
  cat("\n")
}
