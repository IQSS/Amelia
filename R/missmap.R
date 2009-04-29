##
## missmap() - draws a missingness heatmap to show patterns. reorders the
##             variables to put the most missing on the left. reorders the
##             units to unit-period if ts/cs are present.
##
## INPUTS: obj - amelia output (class "amelia")
##         legend - draw a legend? (above the map)
##         col - [1] is the missing color, [2] is observed color
##         main - main title of the plot.
##
## OUPUTS: none
##

missmap <- function(obj, legend = TRUE, col = c("wheat","darkred"), main,
                    y.cex = 0.8, x.cex = 0.8, y.labels, y.at, ...) {
  vnames <- colnames(obj$imputations[[1]])
  n <- nrow(obj$missMatrix)
  p <- ncol(obj$missMatrix)
  
  percent.missing <- colMeans(obj$missMatrix)

  if (!missing(y.labels) &&
      (missing(y.at) && (length(y.labels) != n))) {
    stop("y.at must accompany y.labels if there is less than onefor each row")
  }


  
  if (missing(y.labels)) {
    if (!is.null(obj$arguments$cs)) {
      cs <- obj$imputations[[1]][,obj$arguments$cs]
      if (!is.numeric(cs)) cs <- as.numeric(as.factor(cs))
      if (!is.null(obj$arguments$ts)) {
        ts <- as.numeric(obj$imputations[[1]][,obj$arguments$ts])
        unit.period <- order(cs, ts)
      } else {
        unit.period <- 1:n
      }
      y.labels <- obj$imputations[[1]][,obj$arguments$cs]
      y.labels <- y.labels[unit.period]
      r1 <- obj$missMatrix[unit.period,]
      
      
      brks <- c(TRUE,rep(FALSE, times = (n-1)))
      for (i in 2:n) {
        brks[i] <- (cs[unit.period][i]!=cs[unit.period][i-1])
      }
      y.at <- which(brks)
      y.labels <- y.labels[brks]
    } else {
      r1 <- obj$missMatrix
      y.labels <- row.names(obj$imputations[[1]])
      y.at <- seq(1, n, by=15)
      y.labels <- y.labels[y.at]
    }
  } else {
    r1 <- obj$missMatrix
    if (missing(y.at))
      y.at <- n:1
  }
  missrank <- rev(order(percent.missing))
  
  y.at <- (n:1)[y.at]

  if (missing(main))
    main <- "Missingness Map"
#browser()
  #op <- par(no.readonly=TRUE)
  #if (legend)
  #  par(mar = par("mar") + c(.5,.5,.5,0))
  image(x = 1:(p), y = 1:n, z = t(!r1[n:1,missrank]), axes = FALSE,
        col = col, xlab="", ylab="", main = main)

  axis(1, lwd = 0, labels = vnames[missrank], las = 2, at = 1:p, padj = .5,
       pos = 4, cex.axis = x.cex)
  axis(2, lwd = 0, labels = y.labels, las =2, at = y.at, pos =
       .7, hadj = 1, cex.axis = y.cex)
  if (legend) {
  par(xpd = TRUE)
    legend(x = p*1.07, y = n*1.07, col = col, bty = "n", xjust = 1,
           legend = c("Missing", "Observed"), fill = col, horiz = TRUE)

  #par(op)
  }
  
  
  invisible(NULL)

}
