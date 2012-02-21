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
