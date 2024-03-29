% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine.R
\name{mi.combine}
\alias{mi.combine}
\title{Combine results from analyses on imputed data sets}
\usage{
mi.combine(x, conf.int = FALSE, conf.level = 0.95)
}
\arguments{
\item{x}{List of output from statistical models estimated on
different imputed data sets, as outputted by \code{with(a.out,
expr)} where \code{a.out} is the output of a call to \code{amelia}.}

\item{conf.int}{Logical indicating if confidence intervals should
be computed for each quantity of interest (default is \code{FALSE}).}

\item{conf.level}{The confidence level to use for the confidence
interval  if \code{conf.level = TRUE}. Defaults to 0.95, which
corresponds to a 95 percent confidence interval.}
}
\value{
Returns a \code{tibble} that contains:
\describe{
\item{term}{Name of the coefficient or parameter.}
\item{estimate}{Estimate of the parameter, averagine across imputations.}
\item{std.error}{Standard error of the estimate, accounting for
imputation uncertainty.}
\item{statistic}{Value of the t-statistic for the estimated
parameter.}
\item{p.value}{p-value associated with the test of a null
hypothesis that the true coefficient is zero. Uses the
t-distribution with an imputation-adjusted degrees of freedom.}
\item{df}{Imputation-adjusted degrees of freedom for each
parameter.}
\item{r}{Relative increase in variance due to nonresponse.}
\item{miss.info}{Estimated fraction of missing information.}
\item{conf.low}{Lower bound of the estimated confidence interval.
Only present if \code{conf.int = TRUE}.}
\item{conf.high}{Upper bound of the estimated confidence interval.
Only present if \code{conf.int = TRUE}.}
}
}
\description{
Combine results from statistical models run on multiply imputed
data sets using the so-called Rubin rules.
}
\examples{
data(africa)
a.out <- amelia(x = africa, cs = "country", ts = "year", logs =
"gdp_pc")

imp.mods <- with(a.out, lm(gdp_pc ~ infl + trade))

mi.combine(imp.mods, conf.int = TRUE)

}
\author{
Matt Blackwell
}
