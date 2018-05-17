\name{overimpute}
\alias{overimpute}

\title{Overimputation diagnostic plot}
\description{
Treats each observed value as missing and imputes from the imputation
model from \code{amelia} output.
}
\usage{


overimpute(output, var, draws = 20, subset, legend = TRUE, xlab, ylab, main,
           frontend = FALSE, \dots)

}


\arguments{
  \item{output}{output from the function \code{amelia}.}
  \item{var}{column number or variable name of the variable to
    overimpute.}
  \item{draws}{the number of draws per imputed dataset to generate
  overimputations. Total number of simulations will \code{m * draws}
  where \code{m} is the number of imputations.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the overimputation.}
 \item{legend}{a logical value indicating if a legend should be
    plotted.}
  \item{xlab}{the label for the x-axis. The default is "Observed Values."}
  \item{ylab}{the label for the y-axis. The default is "Imputed Values."}
  \item{main}{main title of the plot. The default is to smartly title the plot
    using the variable name.}
  \item{frontend}{a logical value used internally for the Amelia GUI.}
  \item{\dots}{further graphical parameters for the plot.}
    
}

\details{ This function temporarily treats each observed value in
  \code{var} as missing and imputes that value based on the imputation
  model of \code{output}. The dots are the mean imputation and the
  vertical lines are the 90\% percent confidence intervals for
  imputations of each observed value. The diagonal line is the \eqn{y=x}
  line. If all of the imputations were perfect, then our points would
  all fall on the line. A good imputation model would have about 90\% of
  the confidence intervals containing the truth; that is, about 90\% of
  the vertical lines should cross the diagonal.

  The color of the vertical lines displays the fraction of missing
  observations in the pattern of missingness for that
  observation. The legend codes this information. Obviously, the
  imputations will be much tighter if there are more observed covariates
  to use to impute that observation.

  The \code{subset} argument evaluates in the environment of the
  data. That is, it can but is not required to refer to variables in the
  data frame as if it were attached.
}

\value{A list that contains (1) the row in the original data (\code{row}),
(2) the observed value of that observation (\code{orig}), (2) the mean of the
overimputations (\code{mean.overimputed}), (3) the lower bound of the 95\% confidence interval of
the overimputations (\code{lower.overimputed}), (4) the upper bound of the 95\% confidence interval
of the overimputations (\code{upper.overimputed}), (5) the fraction of the variables that were
missing for that observation in the original data (\code{prcntmiss}),
and (6) a matrix of the raw overimputations, with observations in rows
and the different draws in columns (\code{overimps}).}

\seealso{Other imputation diagnostics are
  \code{\link{compare.density}}, \code{\link{disperse}}, and
  \code{\link{tscsPlot}}.
}
