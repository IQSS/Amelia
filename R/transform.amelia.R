transform.amelia <- function(obj, ...) {
  tcall <- match.call(expand.dots = TRUE)
  if (is.null(obj$transform.calls)) {
    obj$transform.calls <- list(tcall)
  } else {
    obj$transform.calls <- c(obj$transform.calls, tcall)
  }

  tcall[[1]] <- as.name("transform")
  names(tcall)[2] <- ""
  m <- length(obj$imputation)


  for (i in 1:m) {
    tcall[[2]] <- obj$imputations[[i]]
    obj$imputations[[i]] <- eval.parent(tcall)
  }
  return(obj)
}
