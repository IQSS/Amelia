##' Evaluate an R expression in the environments constructed from the
##' imputed data sets of a call to \code{amelia} function. 
##'
##' 
##' @title Execute commands within each imputed data set
##' @param data imputation output from the \code{amelia} funtion. 
##' @param expr expression to evaluate in each imputed data set in
##' \code{data}.
##' @param ... arguments to be passed to (future) methods.
##' @return a list the same length as \code{data$imputations} that
##' contains the output of the expression as evaluated in each imputed
##' data set of \code{data}. 
##' @author Matt Blackwell
##'
##' @examples 
##' data(africa)
##' a.out <- amelia(x = africa, cs = "country", ts = "year", logs =
##' "gdp_pc")
##'
##' imp.mods <- with(a.out, lm(gdp_pc ~ infl + trade))
##'
##' mi.combine(imp.mods, conf.int = TRUE)
##' 
##' @export 
with.amelia <- function(data, expr, ...) {
  expr <- rlang::enquo(expr)
  out <- vector("list", length(data$imputations))
  for (j in seq_along(data$imputations)) {
    out[[j]] <- rlang::eval_tidy(expr, data$imputations[[j]])    
  }
  class(out) <- "amest"
  out
}
