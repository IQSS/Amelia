\name{moPrep}
\alias{moPrep}
\alias{moPrep.molist}
\alias{moPrep.default}


\title{Prepare Multiple Overimputation Settings}
\description{
  A function to generate priors for multiple overimputation of
  a variable measured with error.
}
\usage{
moPrep(x, formula, subset, error.proportion,
       gold.standard = !missing(subset))


}
\arguments{
  \item{x}{either a matrix, data.frame, or a object of class "molist"
    from a previous \code{moPrep} call. The first two derive the priors
    from the data given, and the third will derive the priors from the
    first \code{moPrep} call and add them to the already defined
    priors.}
  \item{formula}{a formula describing the nature of the measurement
    error for the variable. See "Details."}
  \item{subset}{an optional vector specifying a subset of observations
    which possess measurement error.}
  \item{error.proportion}{an optional vector specifying the fraction of
    the observed variance that is due to measurement error.}
  \item{gold.standard}{an optional vector specifying a subset of
    observations which possess no measurement error.}
}
\value{
  An instance of the S3 class "molist" with the following objects:
  \item{priors}{a four-column matrix of the multiple overimputation priors
    associated with the data. Each row of the matrix is \code{c(row,
      column, prior.mean, prior.sd)}. }
  \item{overimp}{a two-column matrix of cells to be overimputed. Each
    row of the matrix is of the form \code{c(row, column)}, which
    indicate the row and column of the cell to be overimputed.}
  \item{data}{the object name of the matrix or data.frame to which
    priors refer.}
  
  Note that \code{priors} and \code{overimp} might contain results from
  multiple calls to \code{moPrep}, not just the most recent. 
}
\details{
}
\references{
Matthew Blackwell, James Honaker, and Gary King, "Multiple
  Overimputation: A Unified Approach to Measurement Error and Missing
  Data" \url{http://gking.harvard.edu/files/abs/measure-abs.shtml}
}
\seealso{
  \code{\link{amelia}}
}
\examples{
data(africa)
m.out <- moPrep(africa, trade ~ trade, error.proportion = 0.1)
a.out <- amelia(m.out, ts = "year", cs = "country")
plot(a.out)
}
\author{James Honaker, Gary King, and Matthew Blackwell}

