\name{tscsPlot}
\alias{tscsPlot}

\title{Plot observed and imputed time-series for a single cross-section}
\description{
  Plots a time series for a given variable in a given cross-section and
  provides confidence intervals for the imputed values.
}
\usage{
tscsPlot(output, var, cs, draws = 100, conf = .90,
         misscol = "red", obscol = "black", xlab, ylab, main,
         pch, ylim, xlim, frontend = FALSE, plotall=FALSE, nr, nc, pdfstub, ...)}


\arguments{
  \item{output}{output from the function \code{amelia}.}
  \item{var}{the column number or variable name of the variable to plot.}
  \item{cs}{the name (or level) of the cross-sectional unit to plot.  
  	Maybe a vector of names which will panel a window of plots}
  \item{draws}{the number of imputations on which to base the confidence
    intervals.}
  \item{conf}{the confidence level of the confidence intervals to plot
    for the imputated values.}
  \item{misscol}{the color of the imputed values and their confidence
    intervals.}
  \item{obscol}{the color of the points for observed units.}
  \item{xlab,ylab,main,pch,ylim,xlim}{various graphical parameters.}
  \item{frontend}{a logical value for use with the \code{AmeliaView} GUI.}
  \item{plotall}{a logical value that provides a shortcut for ploting all unique values of the level.  A shortcut for the \code{cs} argument, a TRUE value overwrites any \code{cs} argument.}
  \item{nr}{the number of rows of plots to use when ploting multiple cross-sectional units.  The default value will try to minimize this value to create a roughly square representation, up to a value of four.  If all plots do not fit on the window, a new window will be started.}
  \item{nc}{the number of columns of plots to use.  See \code{nr}}
  \item{pdfstub}{a stub string used to write pdf copies of each window created by the plot.  The default is not to write pdf output, but any string value will turn on pdf output to the local working directory.  If the stub is \code{mystub}, then plots will be saved as \code{mystub1.pdf}, \code{mystub2.pdf}, etc.}
  \item{\dots}{further graphical parameters for the plot.}
}

\details{
  The \code{cs} argument should be a value from the variable set to the
  \code{cs} argument in the \code{amelia} function for this output. This
  function will not work if the \code{ts} and \code{cs} arguments were
  not set in the \code{amelia} function. If an observation has been
  overimputed, \code{tscsPlot} will plot both an observed and an imputed
  value.
}
