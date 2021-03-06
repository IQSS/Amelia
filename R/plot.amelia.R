
#' Summary plots for Amelia objects
#' 
#' Plots diagnostic plots for the output from the
#' \code{amelia} function.
#'
#' @param x an object of class "amelia"; typically output from the
#'        function \code{amelia}.
#' @param which.vars a vector indicating the variables to plot. The
#'        default is to plot all of the numeric variables that were actually
#'        imputed.
#' @param compare plot the density comparisons for each variable (True or False)
#' @param overimpute plot the overimputation for each variable (True or False)
#' @param ask prompt user before changing pages of a plot (True or False)
#' @param ... further graphical arguments.
#' 
plot.amelia <- function(x, which.vars, compare = TRUE, overimpute =
                        FALSE, ask = TRUE, ...) {

  imputedVars <- colSums(x$missMatrix) > 0

  ## if it's a matrix, it's already numeric
  if (is.data.frame(x$imputations[[1]])) {
    numericVars <- sapply(x$imputations[[1]],"is.numeric")
  } else {
    numericVars <- rep(TRUE, length(imputedVars))
  }

  ## Choose the correct variables to plot. Only numerics.
  ## And, if they didn't pick, only show the imputed variables.
  if (missing(which.vars)) {
    which.vars <- which(imputedVars & numericVars)
  } else {
    ## trim user-choosen variables that are not numeric
    which.vars <- which.vars[numericVars[which.vars]]
  }

  mfrow <- set.mfrow(nvars = length(which.vars), overimpute)

  on.exit(par(NULL))
  layout <- par(mfrow = mfrow)

  for (i in seq(along=which.vars)) {
    if (compare)
      compare.density(output=x, var=which.vars[i], legend=FALSE,...)
    if (overimpute)
      overimpute(output=x, var=which.vars[i])
    if (i==1)
      devAskNewPage(ask)
  }
  devAskNewPage(ask=FALSE)
  invisible()

}

##
## set.mfrow() - gets the proper number of frames for plotting the
##               output of the "amelia" class.
##
## INPUTS: nvars      - number of variables being plotted
##         overimpute - are we plotting overimputes?
##
## OUTPUT: mfrow - vector of length 2 with the (rows,cols) of the
##                 plotting window
##
## NOTICE: idea taken from the "coda" package
##
##

set.mfrow <- function(nvars = 1, overimpute = FALSE) {

  if (overimpute) {
    ## If we are overimputing as well, we need
    ## two plots per variable
    mfrow <- switch(min(nvars, 13),
                    c(2,1), ## 2  plot : 1x2
                    c(2,2), ## 4  plots: 2x2
                    c(3,2), ## 6  plots: 3x2
                    c(4,2), ## 8  plots: 4x2
                    c(3,2), ## 10 plots: 3x2
                    c(3,2), ## 12 plots: 3x2
                    c(4,2), ## 14 plots: 4x2
                    c(4,2), ## 16 plots: 4x2
                    c(4,2), ## 18 plots: 4x2
                    c(3,2), ## 20 plots: 3x2
                    c(3,2), ## 22 plots: 3x2
                    c(3,2), ## 24 plots: 3x2
                    c(4,2)) ## 26 plots: 4x2
  } else {
    mfrow <- switch(min(nvars, 13),
                    c(1,1), ## 1  plot : 1x1
                    c(2,1), ## 2  plots: 2x1
                    c(2,2), ## 3  plots: 2x2
                    c(2,2), ## 4  plots: 2x2
                    c(3,2), ## 5  plots: 3x2
                    c(3,2), ## 6  plots: 3x2
                    c(3,3), ## 7  plots: 3x3
                    c(3,3), ## 8  plots: 3x3
                    c(3,3), ## 9  plots: 3x3
                    c(3,2), ## 10 plots: 3x2
                    c(3,2), ## 11 plots: 3x2
                    c(3,2), ## 12 plots: 3x2
                    c(3,3)) ## 13 plots: 3x3
  }

  return(mfrow)
}
