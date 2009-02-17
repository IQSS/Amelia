\name{plot.amelia}
\alias{plot.amelia}

\title{Summary plots for Amelia objects}

\description{Plots diagnostic plots for the output from the
  \code{amelia} function.}

\usage{

\method{plot}{amelia}(x, which.vars, compare = TRUE, overimpute =
                      FALSE, ask = par("ask"), ...)

}
\arguments{
  \item{x}{an object of class "amelia"; typically output from the
    function \code{amelia}.}
  \item{which.vars}{a vector indicating the variables to plot. The
    default is to plot all of the numeric variables that were actually
    imputed.}
  \item{compare}{plot the density comparisons for each variable?}
  \item{overimpute}{plot the overimputation for each variable?}
  \item{ask}{prompt user before changing pages of a plot?}
  \item{\dots}{further graphical arguments.}
}

\seealso{\code{\link{compare.density}}, \code{\link{overimpute}}}
