\name{transform.amelia}
\alias{transform.amelia}

\title{Transform imputed datasets from Amelia objects}

\description{Updates the imputed datasets from an \code{amelia} output
  with the specified transformations.}

\usage{

\method{tranform}{amelia}(obj, ...)

}
\arguments{
  \item{obj}{an object of class "amelia"; typically output from the
    function \code{amelia}.}
  \item{\dots}{further arguments of the form \code{tag = value}.}
}

\details{The \code{\dots} arugments to \code{transform.amelia} are
  expressions of the form \code{tag = value}, where \code{tag} is the
  variable that is being updated or created and \code{value} is an
  expression that is a function of the variables in the imputed
  datasets. For instance, if you wanted to create an interaction of two
  imputed variables, you could have one argument be \code{intervar =
  var1 * var2}. This would either update the current variable
  \code{intervar} in the imputed data or append a new variable called
  \code{intervar} to the imputed datasets.}

\value{An object of class \code{amelia} with its \code{imputations} and
  \code{missMatrix} values updated according to the transformations. In
  addition, each of the calls to \code{transform.amelia} are stored in
  the \code{transform.calls} item in the returned object.}

\seealso{\code{\link{transform}}}
