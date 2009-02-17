##
## print.amelia() - print method for the "amelia" class
##
## INPUT: object - an object of class "amelia" which is output
##                 from the amelia() function
##
## OUTPUT: Prints some information about the imputations.
##
## mb 02/02/09
##

print.amelia <- function(x, ...) {
  m <- length(x$imputations)
  cat(paste("\nAmelia output with ",m," imputed datasets.\n", sep=""))
  cat(paste("Return code: ", x$code,"\n"), sep="")
  cat(paste("Message: ", x$message, "\n"), sep="")
  cat("\nChain Lengths:\n")
    cat("--------------\n")
  for (i in 1:m) {
    cat(paste("Imputation ",i,":  ", nrow(x$iterHist[[i]]),"\n", sep=""))
  }
  cat("\n")
  invisible(x)
}
