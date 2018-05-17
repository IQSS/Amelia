
#' Summary of an Amelia object
#' 
#' Returns summary information from the Amelia run along with
#' missingles information.
#'
#' @param object an object of class \code{amelia}. Typically, an output
#'        from the function \code{amelia}.
#' @param ... further arguments.
#'
#' @seealso \code{\link{amelia}}, \code{\link{plot.amelia}}
summary.amelia <- function(object, ...) {

  percent.missing <- colMeans(object$missMatrix)
  n.patterns <- nrow(unique(object$missMatrix))

  rows.imputed <- nrow(na.omit(object$imputations[[1]]))
  rows.lwd <- sum(rowSums(object$missMatrix)==0)

  print.amelia(object)

  cat("Rows after Listwise Deletion: ",rows.lwd,"\n")
  cat("Rows after Imputation: ", rows.imputed,"\n")
  cat("Patterns of missingness in the data: ", n.patterns, "\n\n")

  cat("Fraction Missing for original variables: \n")
  cat("-----------------------------------------\n\n")
  tb <- data.frame(cbind(percent.missing))
  rownames(tb) <- colnames(object$missMatrix)
  colnames(tb) <- "Fraction Missing"
  print(tb)
  cat("\n")


  if (!is.null(object$transform.calls)) {
    cat("Post-imputation transformed variables: \n")
    cat("-----------------------------------------\n\n")
    tnames <- unlist(lapply(object$transform.calls,
                            function(x) names(x)[-c(1,2)]))
    texprs <- unlist(lapply(object$transform.calls,
                            function(x) as.character(x[-c(1,2)])))

    tb2 <- data.frame(cbind(texprs))
    rownames(tb2) <- paste(tnames, "=")
    colnames(tb2) <- "Transformations"

    print(tb2)
  }
}
