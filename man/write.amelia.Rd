\name{write.amelia}
\alias{write.amelia}

\title{Write Amelia imputations to file}

\description{Writes the imptuted datasets to file from a run of \code{amelia}.}

\usage{
write.amelia(obj, file.stem, extension = NULL, format = "csv", \dots)

}
\arguments{
  \item{obj}{an object of class "amelia"; typically output from the
    function \code{amelia}.}
  \item{file.stem}{the leading part of the filename to save to
    output The imputation number and \code{extension} will be added to
    complete the filename. This can include a directory path.}
  \item{extension}{the extension of the filename. This is simply what
    follows \code{file.stem} and the imputation number.}
  \item{format}{one of the following output formats: \code{csv},
    \code{dta} or \code{table}. See details.}
  \item{\dots}{further arguments for the \code{write} functions.}
}
\details{
  \code{write.amelia} writes each of the imputed datasets to a file
  using one of the following functions: \code{write.csv},
  \code{write.dta}, or \code{write.table}. You can pass arguments to
  these functions from \code{write.amelia}.

  If you were to set \code{file.stem} to \code{"outdata"} and the
  \code{extension} to \code{".csv"} , then the resulting filename of the
  written files will be
  \preformatted{
    outdata1.csv
    outdata2.csv
    outdata3.csv
    ...
  }
  and so on. 

}


\seealso{\code{\link{write.csv}}, \code{\link{write.table}}, \code{\link{write.dta}}}
