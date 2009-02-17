\name{missmap}
\alias{missmap}

\title{Missingness Map}

\description{Plots a missingness map showing where missingness occurs in
the dataset passed to \code{amelia}.}

\usage{

missmap(obj, legend = TRUE, col = c("darkred","wheat"), main, ...)

}
\arguments{
  \item{obj}{an object of class "amelia"; typically output from the
    function \code{amelia}.}
  \item{legend}{should a legend be drawn?}
  \item{col}{a vector of length two where the first element specifies
    the color for missing cells and the second element specifies
    the color for observed cells.}
  \item{main}{main title of the plot. Defaults to "Missingness Map".}
  \item{\dots}{further graphical arguments.}
}

\details{
  \code{missmap} draws a map of the missingness in a dataset using the
  \code{image} function. The columns are reordered to put the most
  missing variable farthest to the left. The rows are reordered to a
  unit-period order if the \code{ts} and \code{cs} arguments were passed
  to \code{amelia}. If not, the rows are not reordered. 

}

\seealso{\code{\link{compare.density}}, \code{\link{overimpute}},
  \code{\link{tscsPlot}}, \code{\link{image}}, \code{\link{heatmap}}}
