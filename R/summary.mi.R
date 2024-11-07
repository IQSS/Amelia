#' Summary of an mi object
#' 
#' Returns summary information about the list of multiply imputed data
#' sets
#'
#' @param object an object of class \code{mi}. Typically, an output
#'        from the function \code{amelia}.
#' @param ... further arguments.
#'
#' @seealso \code{\link{amelia}}

summary.mi <- function(object, ...) {
  m <- length(object)
  nv <- length(object[[1]])
  nr <- nrow(object[[1]])
  cat("[", m, "imputations,", nv, "variables,", nr, "rows]\n\n")
  summary(do.call(rbind, object))
}
