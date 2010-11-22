\name{amelia}
\alias{amelia}
\alias{amelia.amelia}
\alias{amelia.default}

\title{AMELIA: Multiple Imputation of Incomplete Multivariate Data}
\description{
  Runs the bootstrap EM algorithm on incomplete data and creates
  imputed datasets.  
}
\usage{

\method{amelia}{default}(x, m = 5, p2s = 1,frontend = FALSE, idvars = NULL,
       ts = NULL, cs = NULL, polytime = NULL, splinetime = NULL, intercs = FALSE,
       lags = NULL, leads = NULL, startvals = 0, tolerance = 0.0001,
       logs = NULL, sqrts = NULL, lgstc = NULL, noms = NULL, ords = NULL,
       incheck = TRUE, collect = FALSE, arglist = NULL, empri = NULL,
       priors = NULL, autopri = 0.05, emburn = c(0,0), bounds = NULL,
       max.resample = 100, overimp = NULL, ...)

\method{amelia}{amelia}(x, m = 5, p2s = 1, frontend = FALSE, ...)
\method{amelia}{molist}(x, ...)
} 
\arguments{
  \item{x}{either a matrix, data.frame, a object of class
    "amelia", or an object of class "molist". The first two will call the default S3 method. The third
    a convenient way to perform more imputations with the same
    parameters. The fourth will impute based on the settings from
    \code{moPrep} and any additional arguments.}
  
  \item{m}{the number of imputed datasets to create.}
  
  \item{p2s}{an integer value taking either 0 for no screen output, 1 for normal screen printing of iteration
         numbers, and 2 for detailed screen output.  See "Details" for
         specifics on output when p2s=2.}

  \item{frontend}{a logical value used internally for the GUI.}
  
  \item{idvars}{a vector of column numbers or column names that indicates identification variables.  These will be dropped from the analysis but copied into the imputed datasets.}
    
  \item{ts}{column number or variable name indicating the variable identifying time in time series data.}
  
  \item{cs}{column number or variable name indicating the cross section variable.} 
  
  \item{polytime}{ integer between 0 and 3 indicating what
        power of polynomial should be included in the imputation model
        to account for the effects of time.  A setting of 0 would
        indicate constant levels, 1 would indicate linear time
        effects, 2 would indicate squared effects, and 3 would
        indicate cubic time effects.}

  \item{splinetime}{interger value of 0 or greater to control cubic
  smoothing splines of time. Values between 0 and 3 create a simple
  polynomial of time (identical to the polytime argument). Values \code{k} greater
  than 3 create a spline with an additional \code{k-3}
  knotpoints.}

  \item{intercs}{a logical variable indicating if the
        time effects of \code{polytime} should vary across the
        cross-section.}
  
  \item{lags}{a vector of numbers or names indicating columns in the data
    that should have their lags included in the imputation model.}
  
  \item{leads}{a vector of numbers or names indicating columns in the data
    that should have their leads (future values) included in the imputation 
    model.}

  \item{startvals}{starting values, 0 for the parameter matrix from
    listwise deletion, 1 for an identity matrix.} 

  \item{tolerance}{the convergence threshold for the EM algorithm.}

  \item{logs}{a vector of column numbers or column names that refer 
    to variables that require log-linear transformation. }
    
  \item{sqrts}{a vector of numbers or names indicating columns in the data
    that should be transformed by a sqaure root function.  Data in this
    column cannot be less than zero.}
  
  \item{lgstc}{a vector of numbers or names indicating columns in the data
    that should be transformed by a logistic function for proportional data.
    Data in this column must be between 0 and 1.}
      
  \item{noms}{a vector of numbers or names indicating columns in the data
    that are nominal variables.}

  \item{ords}{a vector of numbers or names indicating columns in the
    data that should be treated as ordinal variables.}

  \item{incheck}{a logical indicating whether or not the inputs to the
    function should be checked before running \code{amelia}.  This should
    only be set to \code{FALSE} if you are extremely confident that your
    settings are non-problematic and you are trying to save computational 
    time.}

  \item{collect}{a logical value indicating whether or
        not the garbage collection frequency should be increased during the imputation model.  Only set this to \code{TRUE} if you are experiencing memory
        issues as it can significantly slow down the imputation
        process.}

  \item{arglist}{an object of class "ameliaArgs" from a previous run of
    Amelia. Including this object will use the arguments from that run.}

  \item{empri}{number indicating level of the empirical (or ridge) prior.
    This prior shrinks the covariances of the data, but keeps the means
    and variances the same for problems of high missingness, small N's or
    large correlations among the variables.  Should be kept small,
    perhaps 0.5 to 1 percent of the rows of the data; a
    reasonable upper bound is around 10 percent of the rows of the
    data.}

  \item{priors}{a four or five column matrix containing the priors for
    either individual missing observations or variable-wide missing
    values.  See "Details" for more information.}

  \item{autopri}{allows the EM chain to increase the empirical prior if
    the path strays into an nonpositive definite covariance matrix, up
    to a maximum empirical prior of the value of this argument times
    $n$, the number of observations.  Must be between 0 and 1, and at
    zero this turns off this feature.}
  
  \item{emburn}{a numeric vector of length 2, where \code{emburn[1]} is
    a the minimum EM chain length and \code{emburn[2]} is the
    maximum EM chain length. These are ignored if they are less than 1.}

  \item{bounds}{a three column matrix to hold logical bounds on the
    imputations. Each row of the matrix should be of the form
    \code{c(column.number, lower.bound,upper.bound)} See Details below.}

  \item{max.resample}{an integer that specifies how many times Amelia
    should redraw the imputed values when trying to meet the logical
    constraints of \code{bounds}. After this value, imputed values are
    set to the bounds.}

  \item{overimp}{a two-column matrix describing which cells are to be
    overimputed. Each row of the matrix should be a \code{c(row,
    column)} pair. Each of these cells will be treated as missing and
  replaced with draws from the imputation model.}

  \item{...}{further arguments to be passed.}

}

\details{ Multiple imputation is a method for analyzing incomplete
  multivariate data.  This function will take an incomplete dataset in
  either data frame or matrix form and return \code{m} imputed datatsets
  with no missing values. The algorithm first bootstraps a sample dataset
  with the same dimensions as the original data, estimates the sufficient statistics (with priors if specified) by EM, and then imputes the missing
  values of sample.  It repeats this process \code{m} times to produce
  the \code{m} complete datasets where the observed values are the same and the unobserved values are drawn from their posterior distributions.

  The function will start a "fresh" run of the algorithm if \code{x} is
  either a incomplete matrix or data.frame. In this method, all of the
  options will be user-defined or set to their default. If \code{x} the output of
  a previous Amelia run (that is, an object of class "amelia"), then
  Amelia will run with the options used in that previous run. This is a
  convenient way to run more imputations of the same model.
  
  You can provide Amelia with informational priors about the missing
  observations in your data.  To specify priors, pass a four or five
  column matrix to the \code{priors} argument with each row specifying a
  different priors as such:

\code{ one.prior <- c(row, column, mean,standard deviation)}

or,

\code{ one.prior <- c(row, column, minimum, maximum, confidence)}.

  So, in the first and second column of the priors matrix should be the
  row and column number of the prior being set.  In the other columns
  should either be the mean and standard deviation of the prior, or a
  minimum, maximum and confidence level for the prior. You must specify
  your priors all as distributions or all as confidence ranges.  Note
  that ranges are converted to distributions, so setting a confidence of
  1 will generate an error.

  Setting a priors for the missing values of an entire variable is done
  in the same manner as above, but inputing a \code{0} for the row
  instead of the row number.  If priors are set for both the entire
  variable and an individual observation, the individual prior takes
  precedence.

  In addition to priors, Amelia allows for logical bounds on
  variables. The \code{bounds} argument should be a matrix with 3
  columns, with each row referring to a logical bound on a variable. The
  first column should be the column number of the variable to be
  bounded, the second column should be the lower bounds for that
  variable, and the third column should be the upper bound for that
  variable. As Amelia enacts these bounds by resampling, particularly
  poor bounds will end up resampling forever. Amelia will stop
  resampling after \code{max.resample} attempts and simply set the
  imputation to the relevant bound. 
  
  If each imputation is taking a long time to converge, you can increase
  the empirical prior, \code{empri}.  This value has the effect of smoothing
  out the likelihood surface so that the EM algorithm can more easily find
  the maximum.  It should be kept as low as possible and only used if needed.
  
  Amelia assumes the data is distributed multivariate normal.  There are a 
  number of variables that can break this assumption.  Usually, though, a 
  transformation can make any variable roughly continuous and unbounded.
  We have included a number of commonly needed transformations for data.
  Note that the data will not be transformed in the output datasets and the
  transformation is simply useful for climbing the likelihood.
  
  Please refer to the Amelia manual for more information on the function
  or the options.
}

\author{James Honaker, Gary King, Matt Blackwell}
 
\value{An instance of S3 class "amelia" with the following objects:
\item{imputations}{a list of length \code{m} with an imputed dataset in
  each entry. The class (matrix or data.frame) of these entries will
  match \code{x}.}
\item{m}{an integer indicating the number of imputations run.}
\item{missMatrix}{a matrix identical in size to the original dataset
  with 1 indicating a missing observation and a 0 indicating an observed
  observation.}
\item{theta}{An array with dimensions \eqn{(p+1)} by \eqn{(p+1)} by \eqn{m}  (where
  \eqn{p} is the number of variables in the imputations model) holding
  the converged parameters for each of the \code{m} EM chains.}
\item{mu}{A \eqn{p} by \eqn{m} matrix of of the posterior modes for the
  complete-data means in each of the EM chains.}
\item{covMatrices}{An array with dimensions \eqn{(p)} by \eqn{(p)} by
  \eqn{m} where the first two dimensions hold the posterior modes of the
  covariance matrix of the complete data for each of the EM chains.}
\item{code}{a integer indicating the exit code of the Amelia run.}
\item{message}{an exit message for the Amelia run}
\item{iterHist}{a list of iteration histories for each EM chain. See
  documentation for details.}
\item{arguments}{a instance of the class "ameliaArgs" which holds the
  arguments used in the Amelia run.}
\item{overvalues}{a vector of values removed for overimputation. Used to
  reformulate the original data from the imputations. }

Note that the \code{theta}, \code{mu} and \code{covMatrcies} objects
refers to the data as seen by the EM algorithm and is thusly centered,
scaled, stacked, tranformed and rearranged. See the manual for details
and how to access this information.

}

\seealso{
  For imputation diagnostics, \code{\link{missmap}}, \code{\link{compare.density}},
  \code{\link{overimpute}} and \code{\link{disperse}}. For time series
  plots, \code{\link{tscsPlot}}. Also: \code{\link{plot.amelia}},
  \code{\link{write.amelia}}, and \code{\link{ameliabind}}. 

}
\examples{
data(africa)
a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
summary(a.out)
plot(a.out)
}

\keyword{models}
