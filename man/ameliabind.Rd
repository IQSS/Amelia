\name{ameliabind}
\alias{ameliabind}

\title{Combine multiple runs of Amelia}

\description{Combines multiple runs of \code{amelia} with the same
  arguments and data into one \code{amelia} object.}

\usage{
ameliabind(...)
}
\arguments{
  \item{...}{one or more objects of class \code{amelia} with the same
  arguments and created from the same data.}
}
\details{
  \code{ameliabind} will combine multiple runs of \code{amelia} into one
  object so that you can utilize diagnostics and modelling on all the
  imputations together. This function is useful for combining multiple
  runs of \code{amelia} run on parallel machines.

  Note that \code{ameliabind} only checks that they arguments and the
  missingness matrix are identical. Thus, it could be fooled by two
  datasets that are identical up to a transformation of one variable. 
}

\value{An object of class \code{amelia}.}


\seealso{\code{\link{amelia}}}
