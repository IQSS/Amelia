
#' Missingness Map
#'
#' Plots a missingness map showing where missingness occurs in
#' the dataset passed to \code{amelia}.
#'
#' @param obj an object of class "amelia"; typically output from the
#'        function \code{amelia}, a matrix or a dataframe.
#' @param vars a vector of column numbers or column names of the data
#'   to include in the plot. The default is to plot all variables. 
#' @param legend should a legend be drawn? (True or False)
#' @param col a vector of length two where the first element specifies
#'        the color for missing cells and the second element specifies
#"        the color for observed cells.
#' @param main main title of the plot. Defaults to "Missingness Map".
#' @param y.cex expansion for the variables names on the x-axis.
#' @param x.cex expansion for the unit names on the y-axis.
#' @param y.labels a vector of row labels to print on the y-axis
#' @param y.at a vector of the same length as \code{y.labels} with row
#'        nmumbers associated with the labels.
#' @param csvar column number or name of the variable corresponding to
#'        the unit indicator. Only used when the \code{obj} is not of class
#'        \code{amelia}.
#' @param tsvar column number or name of the variable corresponding to
#'        the time indicator. Only used when the \code{obj} is not of class
#'        \code{amelia}.
#' @param rank.order a logical value. If \code{TRUE}, the default, then
#'        the order of the variables along the the x-axis is sorted by the
#'        percent missing (from highest to lowest). If \code{FALSE}, it is
#'        simply the order of the variables in the data.
#' @param margins a vector of length two that specifies the bottom and
#'        left margins of the plot. Useful for when variable names or
#'        row names are long.
#' @param gap.xaxis value to pass to the \code{gap.axis} argument of
#'   the \code{axis} function that plots the x-axis. See
#'   \code{\link{axis}} for more details. Ignored on R versions less
#'   than 4.0.0.
#' @param x.las value of the \code{las} argument to pass to the
#'   \code{\link{axis}} function creating the x-axis.
#' @param ... further graphical arguments.
#'
#' @details \code{missmap} draws a map of the missingness in a dataset using the
#' \code{image} function. The columns are reordered to put the most
#' missing variable farthest to the left. The rows are reordered to a
#' unit-period order if the \code{ts} and \code{cs} arguments were passed
#' to \code{amelia}. If not, the rows are not reordered.
#'
#' The \code{y.labels} and \code{y.at} commands can be used to associate
#' labels with rows in the data to identify them in the plot. The y-axis
#' is internally inverted so that the first row of the data is associated
#' with the top-most row of the missingness map. The values of
#' \code{y.at} should refer to the rows of the data, not to any point on
#' the plotting region.
#'
#' @seealso \code{\link{compare.density}}, \code{\link{overimpute}},
#' \code{\link{tscsPlot}}, \code{\link{image}}, \code{\link{heatmap}}
missmap <- function(obj, vars, legend = TRUE, col, main,
                    y.cex = 0.8, x.cex = 0.8, y.labels, y.at, csvar = NULL,
                    tsvar = NULL, rank.order = TRUE, margins = c(5, 5),
                    gap.xaxis = 1, x.las = 2, ...) {


  if (inherits(obj, "amelia")) {
    vnames <- colnames(obj$imputations[[1]])
    n <- nrow(obj$missMatrix)
    p <- ncol(obj$missMatrix)
    percent.missing <- colMeans(obj$missMatrix)
    pmiss.all <- mean(obj$missMatrix)
    r1 <- obj$missMatrix
  } else {
    vnames <- colnames(obj)
    n <- nrow(obj)
    p <- ncol(obj)
    percent.missing <- colMeans(is.na(obj))
    pmiss.all <- mean(is.na(obj))
    r1 <- 1 * is.na(obj)
  }

  if (missing(col)) col <- c("#eff3ff", "#2171b5")
  if (!missing(vars))  {
    if (is.character(vars)) {
      vars <- match(vars, vnames)
      if (any(is.na(vars))) {
        stop("vars not found in the data")
      }
    }
    if (any(!(vars %in% 1:p))) {
      stop("vars outside range of the data")
    }
    p <- length(vars)
    r1 <- r1[, vars]
    percent.missing <- percent.missing[vars]
    pmiss.all <- mean(r1)
  }

  if (!missing(y.labels) &&
      (missing(y.at) && (length(y.labels) != n))) {
    stop("y.at must accompany y.labels if there is less than onefor each row")
  }

  if (is.null(csvar)) csvar <- obj$arguments$cs
  if (is.null(tsvar)) tsvar <- obj$arguments$ts

  if (missing(y.labels)) {
    if (!is.null(csvar)) {
      if (inherits(obj, "amelia")) {
        cs <- obj$imputations[[1]][, csvar]
      } else {
        cs <- obj[, csvar]
      }
      y.labels <- cs
      if (is.factor(y.labels)) y.labels <- levels(y.labels)[unclass(y.labels)]

      cs.names <- y.labels


      if (!is.numeric(cs)) cs <- as.numeric(as.factor(cs))
      if (!is.null(tsvar)) {
        if (inherits(obj, "amelia")) {
          ts <- as.numeric(obj$imputations[[1]][, tsvar])
        } else {
          ts <- as.numeric(obj[, tsvar])
        }
        unit.period <- order(cs, ts)
      } else {
        unit.period <- 1:n
      }

      y.labels <- y.labels[unit.period]
      r1 <- r1[unit.period, ]

      brks <- c(TRUE,rep(FALSE, times = (n-1)))
      for (i in 2:n) {
        brks[i] <- (cs[unit.period][i] != cs[unit.period][i - 1])
      }
      y.at <- which(brks)

      y.labels <- y.labels[brks]
    } else {
      y.labels <- row.names(obj$imputations[[1]])
      y.at <- seq(1, n, by = 15)
      y.labels <- y.labels[y.at]
    }
  } else {
    if (missing(y.at))
      y.at <- n:1
  }
  missrank <- rev(order(percent.missing))
  if (rank.order) {
    chess <- t(!r1[n:1, missrank])
    vnames <- vnames[missrank]
  } else {
    chess <- t(!r1[n:1, ])
  }
  y.at <- (n:1)[y.at]

  if (missing(main))
    main <- "Missingness Map"

  par(mar = c(margins, 2, 1) + 0.1)
  ## here we fork for data/tscs type plots. users cant set this yet.
  type <- "data"
  if (legend) {
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.75, 0.25))
    par(mar = c(margins, 2, 0) + 0.1, mgp = c(3, 0.25, 0))
  }
  if (type == "data") {

    col.fix <- col
    if (sum(!chess) == 0) {
      col.fix <- col[2]
    }
    image(x = 1:(p), y = 1:n, z = chess, axes = FALSE,
          col = col.fix, xlab = "", ylab = "", main = main)
    if (getRversion() >= "4.0.0") {
      axis(1, lwd = 0, labels = vnames, las = x.las, at = 1:p, cex.axis = x.cex,
           gap.axis = gap.xaxis)
    } else {
      axis(1, lwd = 0, labels = vnames, las = x.las, at = 1:p, cex.axis = x.cex)
    }
    axis(2, lwd = 0, labels = y.labels, las = 1, at = y.at, cex.axis = y.cex)


    if (legend) {
      pm.lab <- paste("Missing (", round(100 * pmiss.all), "%)", sep = "")
      po.lab <- paste("Observed (", 100 - round(100 * pmiss.all), "%)",
                      sep = "")
      par(mar = c(0, 0, 0, 0.3))
      plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
      legend("left", col = col, bty = "n", xjust = 0, border = "grey",
             legend = c(pm.lab, po.lab), fill = col, horiz = FALSE)
    }
  } else {
    tscsdata <- data.frame(cs.names, ts, rowMeans(r1))
    tscsdata <- reshape(tscsdata, idvar = "cs.names", timevar = "ts",
                        direction = "wide")
    rownames(tscsdata) <- tscsdata[, 1]
    colnames(tscsdata) <- unique(ts)
    tscsdata <- as.matrix(tscsdata[, -1])

    cols <- rev(heat.colors(5))

    image(z = t(tscsdata), axes = FALSE, col = cols, main = main,
          ylab = "", xlab = "")
    at.seq <- seq(from = 0, to = 1, length = ncol(tscsdata))
    axis(1, labels = unique(ts), at = at.seq, tck = 0, lwd = 0, las = 2)
    axis(2, labels = rownames(tscsdata), at = at.seq, tck = 0, lwd = 0,
         las = 1, cex.axis = .8)

    if (legend) {
      leg.names <- c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1")
      legend(x = 0.95, y = 1.01, col = cols, bty = "n", xjust = 1,
             legend = leg.names, fill = cols, horiz = TRUE)
    }
  }

  invisible(NULL)
}
