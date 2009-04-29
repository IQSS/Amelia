\name{missmap}
\alias{missmap}

\title{Missingness Map}

\description{Plots a missingness map showing where missingness occurs in
the dataset passed to \code{amelia}.}

\usage{

missmap(obj, legend = TRUE, col = c("wheat","darkred"), main,
        y.cex = 0.8, x.cex = 0.8, y.labels, y.at, ...)

}
\arguments{
  \item{obj}{an object of class "amelia"; typically output from the
    function \code{amelia}.}
  \item{legend}{should a legend be drawn?}
  \item{col}{a vector of length two where the first element specifies
    the color for missing cells and the second element specifies
    the color for observed cells.}
  \item{main}{main title of the plot. Defaults to "Missingness Map".}
  \item{x.cex}{expansion for the variables names on the x-axis.}
  \item{y.cex}{expansion for the unit names on the y-axis.}
  \item{y.labels}{a vector of row labels to print on the y-axis}
  \item{y.at}{a vector of the same length as \code{y.labels} with row
    nmumbers associated with the labels.}
  \item{\dots}{further graphical arguments.}
}

\details{
  \code{missmap} draws a map of the missingness in a dataset using the
  \code{image} function. The columns are reordered to put the most
  missing variable farthest to the left. The rows are reordered to a
  unit-period order if the \code{ts} and \code{cs} arguments were passed
  to \code{amelia}. If not, the rows are not reordered.

  The \code{y.labels} and \code{y.at} commands can be used to associate
  labels with rows in the data to identify them in the plot. The y-axis
  is internally inverted so that the first row of the data is associated
  with the top-most row of the missingness map. The values of
  \code{y.at} should refer to the rows of the data, not to any point on
  the plotting region.

}

\seealso{\code{\link{compare.density}}, \code{\link{overimpute}},
  \code{\link{tscsPlot}}, \code{\link{image}}, \code{\link{heatmap}}}
