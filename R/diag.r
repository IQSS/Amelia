##  diag.r
##  amelia diagnostic functins
##
##  05/05/06 mb - added amelia.arg compatibility
##  07/05/06 jh - compare: changed how variable names are found, changed titles/labels, set x-axis values in matplot, colours for no imputations
##                overimpute: added new m-name in output,
##  09/05/06 mb - overimpute: added frontend check for overimpute.
##  15/05/06 jh - overimpute: stacking of original data, and various graphics adjustments
##  01/06/06 mb - added "gethull" and "disperse" for overdispersion diagnostic
##  19/07/06 mb - moved handling of arglists to prep.
##  01/12/06 mb - can't compare non-numerics, only use the relevant columns when
##                building compare
##  13/12/06 mb - changed for new priors.
##  26/03/07 jh - overimpute: excluded polynomials of time from missingness count, reordered ploting of ci's (smallest last), allow variable name as var argument
##  28/03/07 jh - disperse: changed tolerance and empri handling.
##  03/04/07 jh - disperse: changed 1d plot settings, number of colors, minor edits to "patt" construction.
##  10/04/07 jh - created sigalert function to view disperse principal components.
##  22/07/08 mb - good coding update: T->TRUE/F->FALSE
##  10/02/09 mb - compare: added lwd, col, main, lab, etc for user
##                control, added scale so that users can control scaling,
##                uses amelia class
##                overimpute: uses amelia class, added lwd, col, main, lab, etc for user
##                disperse: now uses amelia class
##  02/21/12 jh - added mi.meld to combine multiply imputed quantities of interest and se's.
##  10/30/12 jh - tscsPlot: expanded to allow to cycle through sets of cross sectional units efficiently.


#' Compare observed versus imputed densities
#'
#' Plots smoothed density plots of observed and imputed values from output
#' from the \code{amelia} function.
#'
#' @param output output from the function \code{amelia}.
#' @param var column number or variable name of the variable to plot.
#' @param col a vector of length 2 containing the color to plot the (1)
#'        imputed density and (2) the observed density.
#' @param scaled a logical indicating if the two densities should be
#'        scaled to reflect the difference in number of units in each.
#' @param lwd the line width of the density plots.
#' @param main main title of the plot. The default is to title the plot
#'        using the variable name.
#' @param xlab the label for the x-axis. The default is the name of the
#'        variable.
#' @param ylab the label for the y-axis. The default is "Relative Density."
#' @param legend a logical value indicating if a legend should be
#'        plotted.
#' @param frontend a logical value used internally for the Amelia GUI.
#' @param ... further graphical parameters for the plot.
#'
#' @details   This function first plots a density plot of the observed units for the
#' variable \code{var} in \code{col[2]}. The the function plots a density plot of the mean
#' or modal imputations for the missing units in \code{col[1]}. If a
#' variable is marked "ordinal" or "nominal" with the \code{ords} or
#' \code{noms} options in \code{amelia}, then the modal imputation will
#' be used. If \code{legend} is \code{TRUE}, then a legend is plotted as well.
#'
#' @references
#' Abayomi, K. and Gelman, A. and Levy, M. 2005 "Diagnostics for
#' Multivariate Imputations," \emph{Applied Statistics}. 57,3: 273--291.
#'
#' @examples
#' data(africa)
#'
#' @seealso For more information on how densities are computed,
#' \code{\link{density}}; Other imputation diagnostics are
#' \code{\link{overimpute}}, \code{\link{disperse}}, and
#' \code{\link{tscsPlot}}.
#'
compare.density <- function(output, var, col = c("indianred", "dodgerblue"),
                            scaled = FALSE, lwd = 1, main, xlab, ylab,
                            legend = TRUE, frontend = FALSE, ...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")

  ##data <- getOriginalData(output)
  data <- remove.imputations(output)

  ## Checks on if the variable makes sense to plot.
  if (class(var) == "character")
    if (!(var %in% names(data)))
      stop("The variable name (var) doesn't correspond to a column in the data.")
    else
      var <- match(var,names(data))
  if (any(var > ncol(data), var < 0, (var %% 1) != 0))
    stop("The 'var' option points to a non-existant column.")
  if (var %in% output$arguments$idvar)
    stop("the variable selected was marked as an idvar")

  ## We need to clean the data to make sure that
  ## we're not going to run into NAs
  mcount <- sum(!is.na(output$imputations))
  imputed <- (1:output$m)[!is.na(output$imputations)]

  ## create an empty vector to sum across
  varimp <- matrix(NA, nrow(data), mcount)

  for (i in 1:mcount) {
    if (is.data.frame(data)) {
      varimp[,i] <- output$imputations[[imputed[i]]][[var]]
    } else {
      varimp[,i] <- output$imputations[[imputed[i]]][,var]
    }
  }
  if (var %in% c(output$arguments$noms, output$arguments$ords)) {
    leg.text <- "Modal Imputations"
    varimp <- apply(varimp, 1, function(x) as.numeric(names(which.max(table(x)))))
  } else {
    leg.text <- "Mean Imputations"
    varimp <- rowMeans(varimp)
  }

  if (frontend) {
    dev.new()
  }

  if (is.data.frame(data)) {
    vars <- data[[var]]
  } else {
    vars <- data[,var]
  }

  if (scaled)
    ratio <- sum(is.na(vars))/sum(!is.na(vars))
  else
    ratio <- 1
  varnames <- dimnames(data)[[2]]            # This will work for both data.frames AND matricies.
  vname <- varnames[var]                     # This will work for both data.frames AND matricies.


  if (sum(is.na(vars)) > 0) {
    oiDetect <- (sum(output$missMatrix[,var]) + sum(!is.na(vars))) > length(vars)
    if (missing(main)) {
      if (oiDetect) {
        main <-  paste("Observed and Overimputed values of", vname)
      } else {
        main <- paste("Observed and Imputed values of", vname)
      }
    }
    if (missing(xlab)) {
      xlab <- paste(vname,"  --  Fraction Missing:", round(mean(is.na(vars)), digits = 3))

    }
    if (missing(ylab)) {
      ylab <- "Relative Density"
    }

    xmiss <- density(varimp[output$missMatrix[, var]], na.rm = TRUE)
    xobs  <- density(vars[!is.na(vars)], na.rm = TRUE)
    compplot <- matplot(x = cbind(xmiss$x, xobs$x),
                        y = cbind(ratio * xmiss$y, xobs$y),
                        xlab = xlab, ylab = ylab, type = "l", lwd = lwd,
                        lty = 1, main = main, col = col,...)
    if (legend) {
      legend("topright", legend = c(leg.text, "Observed Values"),
             col = col, lty = c(1,1), bg = 'gray90', lwd = lwd)
    }
  } else {
    if (missing(main)) {
      main <- paste("Observed values of",vname)
    }
    if (missing(xlab)) {
      xlab <- vname
    }
    if (missing(ylab)) {
      ylab <- "Relative Density"
    }

    compplot <- plot(density(varimp, na.rm = TRUE), col = "blue",
                     main = main,...)
    col.none <- c("gray","blue")

    if (legend) {
      legend("topright", legend = c("Mean Imputations (None)","Observed Values"),
             col = col.none, lty = c(1,1), bg = 'gray90')
    }
  }

  invisible()
}



#' Overimputation diagnostic plot
#'
#' Treats each observed value as missing and imputes from the imputation
#' model from \code{amelia} output.
#'
#' @param output output from the function \code{amelia}.
#' @param var column number or variable name of the variable to
#'        overimpute.
#' @param draws the number of draws per imputed dataset to generate
#'   overimputations. Total number of simulations will \code{m *
#'   draws} where \code{m} is the number of imputations.
#' @param subset an optional vector specifying a subset of observations
#'        to be used in the overimputation.
#' @param legend a logical value indicating if a legend should be
#'        plotted.
#' @param xlab the label for the x-axis. The default is "Observed Values."
#' @param ylab the label for the y-axis. The default is "Imputed Values."
#' @param main main title of the plot. The default is to smartly title the plot
#'        using the variable name.
#' @param frontend a logical value used internally for the Amelia GUI.
#' @param ... further graphical parameters for the plot.
#'
#' @details
#' This function temporarily treats each observed value in
#' \code{var} as missing and imputes that value based on the imputation
#' model of \code{output}. The dots are the mean imputation and the
#' vertical lines are the 90\% percent confidence intervals for
#' imputations of each observed value. The diagonal line is the \eqn{y=x}
#' line. If all of the imputations were perfect, then our points would
#' all fall on the line. A good imputation model would have about 90\% of
#' the confidence intervals containing the truth; that is, about 90\% of
#' the vertical lines should cross the diagonal.
#'
#' The color of the vertical lines displays the fraction of missing
#' observations in the pattern of missingness for that
#' observation. The legend codes this information. Obviously, the
#' imputations will be much tighter if there are more observed covariates
#' to use to impute that observation.
#'
#' The \code{subset} argument evaluates in the environment of the
#' data. That is, it can but is not required to refer to variables in the
#' data frame as if it were attached.
#'
#' @return A list that contains (1) the row in the original data
#'   (\code{row}), (2) the observed value of that observation
#'   (\code{orig}), (2) the mean of the overimputations
#'   (\code{mean.overimputed}), (3) the lower bound of the 95\%
#'   confidence interval of the overimputations
#'   (\code{lower.overimputed}), (4) the upper bound of the 95\%
#'   confidence interval of the overimputations
#'   (\code{upper.overimputed}), (5) the fraction of the variables
#'   that were missing for that observation in the original data
#'   (\code{prcntmiss}), and (6) a matrix of the raw overimputations,
#'   with observations in rows and the different draws in columns (\code{overimps}).
#'
#' @seealso Other imputation diagnostics are
#' \code{\link{compare.density}}, \code{\link{disperse}}, and
#' \code{\link{tscsPlot}}.

overimpute <- function(output, var, draws = 20, subset, legend = TRUE, xlab,
                       ylab, main, frontend = FALSE, ...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")


  data <- getOriginalData(output)

  ## via the subset.data.frame function
  if (missing(subset)) {
    r <- TRUE
  } else {
    e <- substitute(subset)
    r <- eval(e, data, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must evaluate to logical")
    }
    r <- r & !is.na(r)
    if (sum(r) == 0) {
      stop("no observations in the subset")
    }
  }
  data <- data[r,]

  origAMr1 <- is.na(data)


  ## Allow character names as arguments for "var" with data.frames

  if(is.character(var)){
    if(!is.data.frame(data)){
      stop("var must be identified by column number as dataset is not a data frame.")
    } else {
      nomnames <- colnames(output$imputations[[1]])[output$arguments$noms]
      if (var %in% nomnames) {
        stop("Cannot overimpute variables set to be nominal")
      }
      varpos <- match(var, colnames(data))
      if(is.na(varpos)){
        stop("The name provided for var argument does not exist in the dataset provided.")
      } else {
        var <- varpos
      }
    }
  }


  ## The argument list for an amelia output is now
  ## at "output$arguments"
  prepped <- amelia.prep(x = data, arglist = output$arguments, incheck = FALSE)

  stacked.var <- match(var, prepped$subset.index[prepped$p.order])
  subset.var <- match(var, prepped$subset.index)
  if (!is.null(prepped$blanks))
    fully.missing <- origAMr1[-prepped$blanks, var][prepped$n.order]
  else
    fully.missing <- origAMr1[, var][prepped$n.order]

  if (is.na(stacked.var)) {
    if (frontend)
      tcltk::tkmessageBox(message="The variable you selected doesn't exist in the Amelia output becuase it wasn't imputed.",icon="error",type="ok")
    stop("var doesn't exist in the amelia output.  It either didn't get imputed or is out of the range of columns.")
  }

  means <- c()
  lowers <- c()
  uppers <- c()
  pcnts <- c()
  color <- c()
  AMr1 <- is.na(prepped$x)
  ## if (sum(!AMr1[,stacked.var]) == 0){
  ##   if (frontend) {
  ##     tkmessageBox(parent = getAmelia("gui"),
  ##                  message="The variable needs to have at least one fully observed cell.",icon="error",type="ok")
  ##   }
  ##   stop("function needs at least one fully observed cell in 'var'.")
  ## }
  AMr1[,stacked.var] <- TRUE
  AMp <- ncol(prepped$x)
  imphold <- matrix(NA, nrow = nrow(prepped$x), ncol = output$m * draws)
  for (i in 1:nrow(prepped$x)) {
    if (fully.missing[i]) {
      next()
    }

    x <- prepped$x[i,,drop=FALSE]
    x[1, stacked.var] <- NA
    o <- !is.na(x)
    miss <- !o
    x[is.na(x)] <- 0
    oo <- 1 * o
    mm <- 1 * miss
                                        #o<-!AMr1[i,]
                                        #o[stacked.var]<-FALSE

    pcntmiss <- (sum(miss))/(length(miss)-sum(prepped$index==0))   # Does not include time polynomials (index==0) in the denominator
    ## These are always fully observed by construction, but auxiliary.
    ## Leaves constructed lags and
    ## leads, and nominal variables
    ## in count, however.

    conf <- c()
    for (k in 1:output$m) {

      ## The theta matrix is now stored in an array with
      ## dimensions c(vars+1,vars+1,m), so this grabs
      ## the kth theta matrix.

      thetareal <- output$theta[,,k]
      xx <- matrix(x, draws, AMp, byrow = TRUE)
      rr <- matrix(AMr1[i,], draws, AMp, byrow = TRUE)
      xc <- .Call("ameliaImpute", xx, rr, oo, mm, c(1, nrow(xx) + 1), thetareal, NULL,
                   NULL, NULL, PACKAGE = "Amelia")
      conf <- c(conf, xc[, stacked.var])
    }

    scaled.conf <- (conf * prepped$scaled.sd[subset.var])  + prepped$scaled.mu[subset.var]
    varlog <- match(var, prepped$logs)

    if (!is.na(varlog)) {
      scaled.conf <- untransform(as.matrix(scaled.conf), logs = 1,
                                 xmin = prepped$xmin[varlog], sqrts = NULL,
                                 lgstc = NULL)
    }
    if (!is.na(match(var,prepped$sqrts))) {
      scaled.conf <- untransform(as.matrix(scaled.conf), logs = NULL,
                                 xmin = NULL, sqrts = 1, lgstc = NULL)
    }
    if (!is.na(match(var,prepped$lgstc))) {
      scaled.conf <- untransform(as.matrix(scaled.conf), logs = NULL,
                                 xmin = NULL, sqrts = NULL, lgstc = 1)
    }

    ##colors are based on rainbow roygbiv l->r is higher missingness  \
    blue <- rgb(0,0,1, alpha = 0.75)
    green <- rgb(0,.75,0, alpha = 0.75)
    orange <- rgb(1, 0.65,0, alpha = 0.75)
    tomato <- rgb(1, 0.39, 0.28, alpha = 0.75)
    red <- rgb(0.75, 0, 0, alpha = 0.75)
    spectrum <- c(blue, green, orange, tomato, red)

    if (pcntmiss < .20)
      color <- c(color, spectrum[1])
    else if (pcntmiss >= .20 && pcntmiss < .40)
      color <- c(color, spectrum[2])
    else if (pcntmiss >= .40 && pcntmiss < .60)
      color <- c(color, spectrum[3])
    else if (pcntmiss >= .60 && pcntmiss < .80)
      color <- c(color, spectrum[4])
    else if (pcntmiss >= .80)
      color <- c(color, spectrum[5])

    imphold[i,] <- scaled.conf
    means <- c(means, mean(scaled.conf))
    lowers <- c(lowers, sort(scaled.conf)[round(output$m * draws * 0.05)])
    uppers <- c(uppers, sort(scaled.conf)[round(output$m * draws * 0.95)])
    pcnts <- c(pcnts, pcntmiss)

  }

                                        #AMr1<-is.na(prepped$x[,stacked.var])
                                        #partial.n.order<-prepped$n.order[!origAMr1]

  if (is.data.frame(data)) {
    xplot <- data[[var]]
  } else {
    xplot <- data[,var]
  }
  if (is.null(prepped$blanks)) {
    xplot <- xplot[prepped$n.order][!fully.missing]
  } else {
    xplot <- xplot[-prepped$blanks][prepped$n.order][!fully.missing]
  }
  addedroom <- (max(uppers) - min(lowers)) * 0.1
  if (!hasArg(log)) {
    this.ylim <- range(c(lowers - addedroom, uppers))
    legpos <- "bottomright"
  } else {
    this.ylim <- range(c(lowers[lowers > 0], uppers + addedroom))
    legpos <- "topright"
  }

  if (missing(xlab)) {
    xlab <- "Observed Values"
  }
  if (missing(ylab)) {
    ylab <- "Imputed Values"
  }
  if (missing(main)) {
    main <- paste("Observed versus Imputed Values of",colnames(data)[var])
  }

  if (frontend) {
    dev.new()
  }
  ci.order <- order(uppers - lowers, decreasing = TRUE)     # Allows smallest CI's to be printed last, and thus not buried in the plot.
  overplot <- plot(xplot[ci.order], means[ci.order], xlab = xlab, ylab = ylab,
                   ylim = this.ylim, type = 'p', main = main,
                   col = color[ci.order], pch = 19,...)
  segments(xplot[ci.order], lowers[ci.order], xplot[ci.order], uppers[ci.order],
           col = color[ci.order])
  if (legend) {
    legend(legpos, legend = c(" 0-.2",".2-.4",".4-.6",".6-.8",".8-1"),
           col = spectrum, lty = c(1,1), horiz = TRUE, bty = "n")
  }

  abline(0,1)

  out <- list(row = prepped$n.order[!fully.missing],
              orig = xplot,
              mean.overimputed = means,
              lower.overimputed = lowers,
              upper.overimputed = uppers,
              prcntmiss = pcnts,
              overimps = imphold[!is.na(imphold[,1]),])
  invisible(out)
}

gethull <- function(st,tol,rots) {
  stvec <- st
  for (i in 1:length(st)) {
    addedvec <- rep(0,length(st))
    addedvec[i] <- tol * 100
    newvec <- cbind(st + addedvec, st - addedvec)
    stvec <- cbind(stvec, newvec)
  }
  reduced.hull <- t(rots) %*% stvec
  return(reduced.hull)
}


#' Overdispersed starting values diagnostic for multiple imputation
#'
#' A visual diagnostic of EM convergence from multiple overdispersed
#' starting values for an output from \code{amelia}.
#'
#' @param output output from the function \code{amelia}.
#' @param m the number of EM chains to run from overdispersed starting values.
#' @param dims the number of principle components of the parameters to
#'        display and assess convergence on (up to 2).
#' @param p2s an integer that controls printing to screen. 0 (default)
#'        indicates no printing, 1 indicates normal screen output and 2
#'        indicates diagnostic output.
#' @param frontend a logical value used internally for the Amelia GUI.
#' @param ... further graphical parameters for the plot.
#'
#' @details   This function tracks the convergence of \code{m} EM chains which start
#' from various overdispersed starting values. This plot should give some
#' indication of the sensitivity of the EM algorithm to the choice of
#' starting values in the imputation model in \code{output}. If all of
#' the lines converge to the same point, then we can be confident that
#' starting values are not affecting the EM algorithm.
#'
#' As the parameter space of the imputation model is of a
#' high-dimension, this plot tracks how the first (and second if
#'                                                 \code{dims} is 2) principle component(s) change over the iterations of
#' the EM algorithm. Thus, the plot is a lower dimensional summary of the
#' convergence and is subject to all the drawbacks inherent in said
#' summaries.
#'
#' For \code{dims==1}, the function plots a horizontal line at the
#' position where the first EM chain converges. Thus, we are checking
#' that the other chains converge close to that horizontal line. For
#' \code{dims==2}, the function draws a convex hull around the point of
#' convergence for the first EM chain. The hull is scaled to be within
#' the tolerance of the EM algorithm. Thus, we should check that the
#' other chains end up in this hull.
#'
#' @seealso Other imputation diagnostics are
#' \code{\link{compare.density}}, \code{\link{disperse}}, and
#' \code{\link{tscsPlot}}
disperse <- function(output, m = 5, dims = 1, p2s = 0, frontend = FALSE,...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")

  ## The original data is the imputed data with the
  ## imputations marked to NA. These two lines do that
  data <- getOriginalData(output)

  if (frontend) {
    requireNamespace("tcltk")
    putAmelia("output.log", c(getAmelia("output.log"), "==== Overdispersion Output ====\n"))
  }

                                        # prep the data and arguments
  prepped<-amelia.prep(x=data, arglist=output$arguments)

  if (p2s) cat("-- Imputation", "1", "--")
  if (frontend) {
    putAmelia("output.log", c(getAmelia("output.log"), paste("-- Imputation","1","--\n")))
  }
  flush.console()

                                        # run EM, but return it with the theta at each iteration
  thetanew <- emarch(prepped$x, p2s = p2s, thetaold = NULL,
                     tolerance = prepped$tolerance, startvals = 0,
                     priors = prepped$priors, empri = prepped$empri,
                     frontend = frontend, allthetas = TRUE, collect = FALSE)  #change 4

                                        # thetanew is a matrix whose columns are vectorized upper triangles of theta
                                        # matrices for each iteration. thus, there are k(k+1)/2 rows.
  impdata <- thetanew$thetanew

                                        # we'll put the theta of the last iteration into a new starting theta
  startsmat <- matrix(0, ncol(prepped$x) + 1, ncol(prepped$x) + 1)
  startsmat[upper.tri(startsmat, TRUE)] <- c(-1, impdata[, ncol(impdata)])
  startsmat <- t(startsmat)
  startsmat[upper.tri(startsmat, TRUE)] <- c(-1, impdata[, ncol(impdata)])
  iters <- nrow(thetanew$iter.hist) + 1

  for (i in 2:m) {
    if (p2s) cat("-- Imputation", i, "--\n")
    if (frontend) {
      putAmelia("output.log", c(getAmelia("output.log"), paste("-- Imputation",i,"--\n")))
    }

                                        # get a noisy sample of data from the that starting value (which is the
                                        # Amelia answer) and use that to estimate a new starting theta (mus/vcov)
    newstarts <- rmvnorm(round(2.5 * ncol(prepped$x)),
                         startsmat[1,2:ncol(startsmat)],
                         startsmat[2:nrow(startsmat),2:nrow(startsmat)])
    startcov <- var(newstarts)
    startmus <- colMeans(newstarts)
    newstartsmat <- matrix(-1, ncol(prepped$x) + 1, ncol(prepped$x) + 1)
    newstartsmat[2:nrow(startsmat),2:nrow(startsmat)] <- startcov
    newstartsmat[1,2:nrow(startsmat)] <- startmus
    newstartsmat[2:nrow(startsmat),1] <- startmus

                                        # grab the iteration history of the thetas
    thetanew <- emarch(prepped$x, p2s = p2s, thetaold = newstartsmat,
                       tolerance = prepped$tolerance, startvals = 0,
                       priors = prepped$priors, empri = prepped$empri,
                       frontend = frontend, allthetas = TRUE, collect = FALSE)  # change 5
    impdata <- cbind(impdata, thetanew$thetanew)
    iters <- c(iters, nrow(thetanew$iter.hist) + 1)
  }
  if (dims == 1)
    comps <- c(1)
  else
    comps <- c(1,2)

                                        # reduce the dimenionality from k(k+1)/2 to 1 or 2 via principle components
  rotations <- prcomp(t(impdata))$rotation[, comps]
  reduced.imps <- t(rotations) %*% impdata

  cols <- rainbow(m)
                                        # plot the imputations
  if (frontend) {
    dev.new()
  }
  if (dims == 1) {
    addedroom <- (max(reduced.imps) - min(reduced.imps)) * 0.1
    x <- seq(iters[1])
    y <- reduced.imps[1, 1:iters[1]]
    patt <- seq(1, length(x) - 1)
    plot(x, y, col = 1, main = "Overdispersed Start Values",
         xlab = "Number of Iterations", ylab = "Largest Principle Component",
         xlim = c(0, max(iters)),
         ylim = range(c(reduced.imps - addedroom, reduced.imps)), type = "n")
    segments(x[patt], y[patt], x[patt + 1], y[patt + 1], col = cols[1])
    for (i in 2:length(iters)) {
      x <- seq(iters[i])
      y <- reduced.imps[1, (sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      patt <- seq(1, length(x)-1)
      segments(x[patt], y[patt], x[patt+1], y[patt+1], col=cols[i])
                                        #points(x,y,col=i)
    }
    abline(h = reduced.imps[iters[1]], lwd = 2)
    legend("bottomright", legend = c("Convergence of original starting values"),
           lwd = 2, bty = "n")
  } else {
    xrange <- c((min(reduced.imps[1,])), (max(reduced.imps[1,])))
    yrange <- c((min(reduced.imps[2,])), (max(reduced.imps[2,])))
    plot(reduced.imps[1,1:iters[1]], reduced.imps[2,1:iters[1]], type = "n",
         main = "Overdispersed Starting Values",
         xlab = "First Principle Component", ylab = "Second Principle Component",
         col=cols[1], xlim = xrange, ylim = yrange)
    for (i in 2:length(iters)) {
      x <- reduced.imps[1, (sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      y <- reduced.imps[2, (sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      patt <- c()
      xdiffs <- diff(x)
      ydiffs <- diff(y)
      veclength <- sqrt(xdiffs^2+ydiffs^2)
      for (j in 1:length(xdiffs))
        if (veclength[j] > xinch(1/500))
          patt <- c(patt,j)
      if (!is.null(patt))
        arrows(x[patt], y[patt], x[patt + 1], y[patt + 1], length = .1,
               col = cols[i])
      patt <- seq(1, length(x) - 1)
      segments(x[patt], y[patt], x[patt+1], y[patt+1], col = cols[i])
    }
    x <- reduced.imps[1,1:iters[1]]
    y <- reduced.imps[2,1:iters[1]]
    xdiffs <- diff(x)
    ydiffs <- diff(y)
    veclength <- sqrt(xdiffs^2+ydiffs^2)
    inchlength <- sqrt(sum(xyinch(1/500)^2))
    patt <- c()
    for (j in 1:length(xdiffs))
      if (veclength[j] > inchlength)
        patt <- c(patt,j)
                                        #if (!is.null(patt))
                                        #  arrows(x[patt],y[patt],x[patt+1],y[patt+1],length=.15,col=1,lwd=5)
    patt <- seq(1, length(x)  -1)
    segments(x[patt], y[patt], x[patt + 1], y[patt + 1], col = cols[1], lwd = 1)
    dists <- gethull(st = impdata[ ,iters[1]], tol = prepped$tolerance,
                     rots = rotations)
    convexhull <- chull(t(dists))
    convexhull <- c(convexhull, convexhull[1])
    lines(t(dists)[convexhull,], col = "orange", pch = 19, lwd = 2)
    abline(h = 0, lty = 2)
    abline(v = 0, lty = 2)
  }
                                        #if (frontend)
                                        #  tkdestroy(getAmelia("tcl.window"))

  out <- list(impdat = impdata, p.order = prepped$p.order, index = prepped$index,
              iters = iters, rotations = rotations, dims = dims)
  invisible(out)

}


sigalert <- function(data, disperse.list, output, notorious = 5){

  k <- length(disperse.list$p.order) + 1

                                        # Construct Variable Names for all variables constructed in Imputation Model.
                                        # This uses the "index" which details all the variables included in the imputation model.
                                        # The index is in the unstacked variable order.
                                        # Possibly, if this is useful elsewhere, this might be moved to "prep.r".

  varnm <- NULL
  lag.count <- 0
  lead.count <- 0
  poly.count <- 0
  unknown.count <- 0

  for (i in 1:(k-1)) {
    if (identical(disperse.list$index[i], -0.5)) {
      lag.count <- lag.count + 1
      varnm <- c(varnm, paste("lag", lag.count))
    } else if (identical(disperse.list$index[i], 0.5)) {
      lead.count <- lead.count + 1
      varnm <- c(varnm, paste("lead", lead.count))
    } else if (identical(disperse.list$index[i],0)) {
      poly.count <- poly.count + 1
      varnm <- c(varnm, paste("polytime", poly.count))
    } else if(disperse.list$index[i] >= 1) {
      varnm <- c(varnm, names(data[disperse.list$index[i]]))      # Check what this does with matricies?
    } else {
      unknown.count <- unknown.count + 1
      varnm <- c(varnm, paste("unknown", unknown.count))
    }
  }

                                        #  WARNING: Currently assumes rotations is a vector.  If dim=2, rotations is a matrix.
                                        #  if(!identical(disperse.list$dims,1)){
                                        #    disperse.list$rotations<-disperse.list$rotations[1,]
                                        #  }

                                        # This is a flag vector that identifies the largest values in the first principal component.

  largest.rotations <- disperse.list$rotations * 0
  largest.rotations[order(abs(disperse.list$rotations),decreasing = TRUE)[1:notorious]] <- 1

                                        # This is a matrix of the size of theta, which has a 1 in the positions of the largest
                                        # contributions to the first principal component.
                                        # (largest corresponding elements of disperse.list$rotations)

  map <- matrix(0, k, k)
  map[upper.tri(map, TRUE)] <- c(0, largest.rotations)
  map <- t(map)
  map[upper.tri(map, TRUE)] <- c(0, largest.rotations)
  map[c(1, disperse.list$p.order + 1), c(1, disperse.list$p.order + 1)] <- map                         # Rearrange to unstacked variable positions

  print(abs(map))

  gtz<-function(a)
    return(sum(a) > 0)
  row.keep <- apply(map, 1, gtz)
  col.keep <- apply(map, 2, gtz)

                                        # This is the submatrix of rotations, reshaped as a theta matrix, with the largest elements.

  prcomp.matrix <- matrix(0,k,k)
  prcomp.matrix[upper.tri(prcomp.matrix, TRUE)] <- c(0, disperse.list$rotations)
  prcomp.matrix <- t(prcomp.matrix)
  prcomp.matrix[upper.tri(prcomp.matrix, TRUE)] <- c(0, disperse.list$rotations)
  prcomp.matrix[c(1,disperse.list$p.order+1),c(1,disperse.list$p.order+1)] <- prcomp.matrix     # Rearrange to unstacked variable positions

                                        # This is the submatrix that we want to represent
  portal <- prcomp.matrix[row.keep,col.keep]
  portalsize <- ncol(portal)

  portal.row.names <- varnm[row.keep]               # In symmetric matricies, these are the same.
  portal.col.names <- varnm[col.keep]               # In symmetric matricies, these are the same.

                                        # This is a matrix that gives the relative rank of every element.
  col.map <- matrix(0, portalsize, portalsize)
  col.portal <- rank(abs(portal[upper.tri(portal, TRUE)]))
  col.map[upper.tri(col.map, TRUE)] <- col.portal
  col.map <- t(col.map)
  col.map[upper.tri(col.map, TRUE)] <- col.portal

                                        # This creates a continuous color palette of the correct size.
  n.unique <- sum(upper.tri(matrix(1, portalsize, portalsize), TRUE))
  Lab.palette <- colorRampPalette(c("white", "yellow", "red"), space = "Lab")
  my.palette <- Lab.palette(n.unique)

                                        # Plot the submatrix to be represented.

  plot.new()
  plot.window(xlim = c(-2, portalsize + 1), ylim = c(1, portalsize + 3))
  for(i in 1:portalsize){
    text(x = 1, y = portalsize - i + 1 + 0.5, pos = 2,
         labels = portal.row.names[i])        # Row variable names
    for(j in 1:portalsize){
      rect(xleft = j, ybottom = portalsize - i + 1, xright = j + 1,
           ytop = portalsize - i + 2, density = NULL, angle = 45,
           col = my.palette[col.map[i, j]], border = NULL, lty = par("lty"),
           lwd = par("lwd"))
      text(x = j + 0.5, y = portalsize - i + 1 + 0.5,
           labels = as.character(round(portal[i,j]*100)/100) )      # SHOULD FIND BETTER SIG FIGS HACK
    }
  }
  for(j in 1:portalsize){
    text(x = j + 0.6, y = portalsize + 1.1, pos = 2,
         labels = portal.col.names[j], srt = 270)  # Column variable names.
  }

  return(NULL)
}


#' Plot observed and imputed time-series for a single cross-section
#'
#' Plots a time series for a given variable in a given cross-section and
#' provides confidence intervals for the imputed values.
#'
#' @param output output from the function \code{amelia}.
#' @param var the column number or variable name of the variable to plot.
#' @param cs the name (or level) of the cross-sectional unit to plot.
#'           Maybe a vector of names which will panel a window of plots
#' @param draws the number of imputations on which to base the confidence
#'        intervals.
#' @param conf the confidence level of the confidence intervals to plot
#'        for the imputated values.
#' @param misscol the color of the imputed values and their confidence
#'        intervals.
#' @param obscol the color of the points for observed units.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param main overall plot title
#' @param pch point shapes for the plot.
#' @param ylim y limits (y1, y2) of the plot.
#' @param xlim x limits (x1, x2) of the plot.
#' @param frontend a logical value for use with the \code{AmeliaView} GUI.
#' @param plotall a logical value that provides a shortcut for ploting all unique values of the level.
#'        A shortcut for the \code{cs} argument, a TRUE value overwrites any
#'        \code{cs} argument.
#' @param nr the number of rows of plots to use when ploting multiple cross-sectional
#'        units.  The default value will try to minimize this value to create a roughly
#'        square representation, up to a value of four.  If all plots do not fit on the
#'        window, a new window will be started.
#' @param nc the number of columns of plots to use.  See \code{nr}
#' @param pdfstub a stub string used to write pdf copies of each window created by the
#'        plot.  The default is not to write pdf output, but any string value will turn
#'        on pdf output to the local working directory.  If the stub is \code{mystub},
#'        then plots will be saved as \code{mystub1.pdf}, \code{mystub2.pdf}, etc.
#' @param ... further graphical parameters for the plot.
#'
#' @details
#'   The \code{cs} argument should be a value from the variable set to the
#'  \code{cs} argument in the \code{amelia} function for this output. This
#'  function will not work if the \code{ts} and \code{cs} arguments were
#'  not set in the \code{amelia} function. If an observation has been
#'  overimputed, \code{tscsPlot} will plot both an observed and an imputed
#'  value.

tscsPlot <- function(output, var, cs, draws = 100, conf = .90,
                     misscol = "red", obscol = "black", xlab, ylab, main,
                     pch, ylim, xlim, frontend = FALSE, plotall=FALSE, nr, nc, pdfstub, ...) {
  if (missing(var))
    stop("I don't know which variable (var) to plot")
  if (missing(cs) && !plotall)
    stop("case name (cs) is not specified")
  if (is.null(output$arguments$ts) || is.null(output$arguments$cs))
    stop("both 'ts' and 'cs' need to be set in the amelia output")
  if (!("amelia" %in% class(output)))
    stop("the 'output' is not Amelia output")

  data <- getOriginalData(output)

                                        # Allow character names as arguments for "var" with data.frames

  if (is.character(var)) {
    if (!is.data.frame(data)) {
      stop("'var' must be identified by column number as dataset is not a data frame")
    } else {
      varpos <- match(var, colnames(data))
      if (is.na(varpos)) {
        stop("the name provided for 'var' argument does not exist in the dataset provided")
      } else {
        var <- varpos
      }
    }
  }

  csvarname <- output$arguments$cs
  tsvarname <- output$arguments$ts
  if (is.data.frame(data)) {
    csvar <- data[[csvarname]]
    tsvar <- data[[tsvarname]]
  } else {
    csvar <- data[,output$arguments$cs]
    tsvar <- data[,output$arguments$ts]
  }

  if (is.factor(csvar)) {
      units <- levels(csvar)
  } else {
      units <- unique(csvar)
  }

  if (plotall) {
        cs <- units
  } else {
    if (!(all(cs %in% units)))
      stop("some cross-section unit requested for the plot is not in the data")
  }

# Picks a number of rows and columns if not user defined.  Maxs out at 4-by-4, unless user defined
  if (missing(nr)) {
    nr <- min(4, ceiling(sqrt(length(cs))))
  }
  if (missing(nc)) {
    nc <- min(4, ceiling(length(cs)/nr))
  }

  if (length(cs)>1) {
    oldmfcol <- par()$mfcol
    par(mfcol = c(nr, nc))
  }

  prepped <- amelia.prep(x = data, arglist = output$arguments)
  if (!is.null(prepped$blanks)) {
    data <- data[-prepped$blanks,]
    unit.rows <- which(csvar %in% cs)
    miss <- output$missMatrix[-prepped$blanks,][unit.rows, var] == 1
  } else {
    unit.rows <- which(csvar %in% cs)
    miss <- output$missMatrix[unit.rows, var] == 1
  }

  time <- tsvar[unit.rows]   # These are the time values for rows appearing in some future plot
  imps.cs <- csvar[unit.rows]   # These are the cs units for rows appearing in some future plot
  cross.sec <- prepped$x[!is.na(match(prepped$n.order, unit.rows)),]
  stacked.var <- match(var, prepped$subset.index[prepped$p.order])
  subset.var <- match(var, prepped$subset.index)
  imps <- array(NA, dim = c(nrow(cross.sec), draws))


  drawsperimp <- draws/output$m
  if (sum(miss) > 0) {
    for (i in 1:draws) {
      currtheta <- output$theta[,,ceiling(i/drawsperimp)]
      imps[,i] <- amelia.impute(x = cross.sec, thetareal = currtheta,
                                bounds = prepped$bounds,
                                priors = prepped$priors,
                                max.resample = output$arguments$max.resample)[,stacked.var]
    }

    imps <- imps*prepped$scaled.sd[subset.var] + prepped$scaled.mu[subset.var]
    if (var %in% output$arguments$logs) {
      imps <- exp(imps) + prepped$xmin[which(var == output$arguments$logs)]
    }
    if (var %in% output$arguments$sqrt) {
      imps <- imps^2
    }
    if (var %in% output$arguments$lgstc) {
      imps <- exp(imps)/(1 + exp(imps))
    }

    outoforder <- match(prepped$n.order, unit.rows)[!is.na(match(prepped$n.order, unit.rows))]
    imps <- imps[order(outoforder),]
  }

  if (missing(pch)) pch <- 19
  if (missing(xlab)) xlab <- "time"
  if (missing(ylab)) ylab <- names(data)[var]

  if (frontend) {
    dev.new()
  }

  if (!missing(main)) {
    main <- rep(main, length.out = length(cs))
  }
  count <- 0
  for(i in 1:length(cs)){

      current.rows <- which(csvar == cs[i])
      current.time <- tsvar[current.rows]

      flag <- imps.cs == cs[i]
      current.miss <- miss[flag]

      if (sum(current.miss) > 0) {
        current.imps <- imps[flag,]
        current.means <- rowMeans(current.imps)
        current.uppers <- apply(current.imps, 1, quantile, probs = (conf + (1 - conf)/2))   # THIS IS LIKELY SLOW
        current.lowers <- apply(current.imps, 1, quantile, probs = (1-conf)/2)              # THIS IS LIKELY SLOW
      } else {
        current.means <- data[[var]][current.rows]
        current.uppers <- current.lowers <- current.means
      }

      cols <- ifelse(current.miss, misscol, obscol)
      current.main <- ifelse(missing(main), as.character(cs[i]), main[i])  # Allow title to be rolling if not defined
      if (missing(xlim)) {                                # Allow axes to vary by unit, if not defined
        current.xlim <- range(current.time)
      } else {
        current.xlim <- xlim
      }
      if (missing(ylim)) {
        current.ylim <- range(current.uppers,current.lowers,current.means)
      } else {
        current.ylim <- ylim
      }

    plot(x = current.time, y = current.means, col = cols, pch = pch,
         ylim = current.ylim, xlim = current.xlim, ylab = ylab, xlab = xlab,
         main = current.main, ...)
    segments(x0 = current.time, x1 = current.time, y0 = current.lowers,
             y1 = current.uppers, col = cols, ...)

    oiDetect <- (sum(output$missMatrix[current.rows,var]) +
                   sum(!is.na(data[current.rows, var]))) > length(current.rows)
    if (oiDetect) {
      points(x = current.time, y = data[current.rows, var], pch = pch,
             col = obscol)
    }

    # print page if window full
    if ((!missing(pdfstub)) & (i %% (nr*nc) ==0)) {
      count <- count + 1
      dev.copy2pdf(file = paste(pdfstub, count, ".pdf", sep=""))
    }
  }

  if (!missing(pdfstub)) {
    if ((i %% (nr*nc)) != 0) {           # print last page if not complete
      count <- count + 1
      dev.copy2pdf(file = paste(pdfstub, count, ".pdf", sep=""))
    }
    par(mfcol = oldmfcol)             # return to previous windowing
  }                                 # although always now fills by col even if previously by row

  invisible(imps)

}


#' Combine Multiple Results From Multiply Imputed Datasets
#'
#' Combine sets of estimates (and their standard errors) generated from
#' different multiply imputed datasets into one set of results.
#'
#' @param q A matrix or data frame of (k) quantities of interest (eg.
#'        coefficients, parameters, means) from (m) multiply imputed datasets.
#'        Default is to assume the matrix is m-by-k (see \code{byrow}), thus each
#'        row represents a set of results from one dataset, and each column
#'        represents the different values of a particular quantity of interest
#'        across the imputed datasets.
#' @param se A matrix or data frame of standard errors that correspond to each of the
#'        elements of the quantities of interest in \code{q}.  Should be the same
#'        dimensions as \code{q}.
#' @param byrow logical.  If \code{TRUE}, \code{q} and \code{se} are treated as
#'        though each row represents the set of results from one dataset
#'        (thus m-by-k).  If \code{FALSE}, each column represents results from one
#'        dataset (thus k-by-m).
#'
#' @details Uses Rubin's rules for combining a set of results from multiply imputed
#' datasets to reflect the average result, with standard errors that both average
#' uncertainty across models and account for disagreement in the estimated values
#' across the models.
#'
#' @return
#'   \item{q.mi}{Average value of each quantity of interest across the m models}
#'   \item{se.mi}{Standard errors of each quantity of interest}
#'
#' @references
#' Rubin, D. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}.
#' New York: Wiley.
#'
#' Honaker, J., King, G., Honaker, J. Joseph, A. Scheve K. (2001). Analyzing
#' Incomplete Political Science Data: An Alternative Algorithm for Multiple
#' Imputation \emph{American Political Science Review}, \bold{95(1)}, 49--69. (p53)
#'
mi.meld<-function(q, se, byrow = TRUE) {

  if (!byrow) {
    q <- t(q)
    se <- t(se)
  }

  if (is.data.frame(q)) {
    q <- as.matrix(q)
  }

  if (is.data.frame(se)) {
    se <- as.matrix(se)
  }

  am.m <- nrow(q)

  ones <- matrix(1, nrow = 1, ncol = am.m)
  imp.q <- (ones %*% q)/am.m        # Slightly faster than "apply(b,2,mean)"
  ave.se2 <- (ones %*% (se^2))/am.m # Similarly, faster than "apply(se^2,2,mean)"
  diff <- q - matrix(1, nrow = am.m, ncol = 1) %*% imp.q
  sq2 <- (ones %*% (diff^2))/(am.m - 1)
  imp.se <- sqrt(ave.se2 + sq2 * (1 + 1/am.m))

  return(list(q.mi = imp.q, se.mi = imp.se))

}
