#' Transform imputed datasets from Amelia objects}
#'
#' @description Updates the imputed datasets from an \code{amelia} output
#'              with the specified transformations.
#'  
#' @param {_data}an object of class "amelia"; typically output from the
#'        function \code{amelia}.
#' @param {\dots}further arguments of the form \code{tag = value}.
#'
#' @param {The \code{\dots} arugments to \code{transform.amelia} are
#'  expressions of the form \code{tag = value}, where \code{tag} is the
#'  variable that is being updated or created and \code{value} is an
#'  expression that is a function of the variables in the imputed
#'  datasets. For instance, if you wanted to create an interaction of two
#'  imputed variables, you could have one argument be \code{intervar =
#'    var1 * var2}. This would either update the current variable
#'  \code{intervar} in the imputed data or append a new variable called
#'  \code{intervar} to the imputed datasets.
#'
#' \value An object of class \code{amelia} with its \code{imputations} and
#'  \code{missMatrix} values updated according to the transformations. In
#'  addition, each of the calls to \code{transform.amelia} are stored in
#'  the \code{transform.calls} item in the returned object.
#'
#' @seealso {\code{\link{transform}}}








transform.amelia <- function(`_data`, ...) {
  tcall <- match.call(expand.dots = TRUE)
  if (is.null(`_data`$transform.calls)) {
    `_data`$transform.calls <- list(tcall)
  } else {
    `_data`$transform.calls <- c(`_data`$transform.calls, tcall)
  }


  tcall[[1]] <- as.name("transform")
  names(tcall)[2] <- ""
  m <- length(`_data`$imputation)

  orig.data <- remove.imputations(`_data`)
  tcall[[2]] <- orig.data
  new.miss.matrix <- as.matrix(is.na(eval.parent(tcall)))

  for (i in 1:m) {
    tcall[[2]] <- `_data`$imputations[[i]]
    `_data`$imputations[[i]] <- eval.parent(tcall)
  }
  `_data`$missMatrix <- new.miss.matrix
  return(`_data`)
}
