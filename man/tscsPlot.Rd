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
         pch, ylim, xlim, frontend = FALSE, ...)}


\arguments{
  \item{output}{output from the function \code{amelia}.}
  \item{var}{the column number or variable name of the variable to plot.}
  \item{cs}{the name of the cross-section to plot.}
  \item{draws}{the number of imputations on which to base the confidence
    intervals.}
  \item{conf}{the confidence level of the confidence intervals to plot
    for the imputated values.}
  \item{misscol}{the color of the imputed values and their confidence
    intervals.}
  \item{obscol}{the color of the points for observed units.}
  \item{xlab,ylab,main,pch,ylim,xlim}{various graphical parameters.}
  \item{frontend}{a logical value for use with the \code{AmeliaView} GUI.}
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
