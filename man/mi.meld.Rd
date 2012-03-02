\name{mi.meld}
\alias{mi.meld}

\title{Combine Multiple Results From Multiply Imputed Datasets}
\description{
  Combine sets of estimates (and their standard errors) generated from different multiply imputed datasets into one set of results.
}
\usage{
mi.meld(q, se, byrow = TRUE)
}

\arguments{
  \item{q}{A matrix or data frame of (k) quantities of interest (eg. coefficients, parameters, means) from (m) multiply imputed datasets.  Default is to assume the matrix is m-by-k (see \code{byrow}), thus each row represents a set of results from one dataset, and each column represents the different values of a particular quantity of interest across the imputed datasets.}
  \item{se}{A matrix or data frame of standard errors that correspond to each of the elements of the quantities of interest in b.  Should be the same dimensions as \code{b}.}
  \item{byrow}{logical.  If \code{TRUE}, b and se are treated as though each row represents the set of results from one dataset 
(thus m-by-k).  If \code{FALSE}, each column represents results from one dataset (thus k-by-m).}
}

\details{
  Uses Rubin's rules for combining a set of results from multiply imputed datasets to reflect the average result, with standard errors that both average uncertainty across models and account for disagreement in the estimated values across the models.
}

\value{
  \item{q.mi}{Average value of each quantity of interest across the m models}
  \item{se.mi}{Standard errors of each quantity of interest}
}

\references{
Rubin, D. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}.  New York: Wiley.

Honaker, J., King, G., Honaker, J. Joseph, A. Scheve K. (2001). Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation \emph{American Political Science Review}, \bold{95(1)}, 49--69. (p53)
}

\examples{
data(africa)
m <- 5
a.out <- amelia(x = africa, m=m, cs = "country", ts = "year", logs = "gdp_pc")

b.out<-NULL
se.out<-NULL
for(i in 1:m) {
  ols.out <- lm(civlib ~ trade ,data = a.out$imputations[[i]])
  b.out <- rbind(b.out, ols.out$coef)
  se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}

combined.results <- mi.meld(q = b.out, se = se.out)
}