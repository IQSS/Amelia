\name{amelia}
\alias{amelia}

\title{AMELIA: Mutliple Imputation of Incomplete Multivariate Data}
\description{
  Runs the bootstrap EM algorithm on incomplete data and creates
  imputed datasets.  
}
\usage{

amelia(data,m=5,p2s=1,frontend=FALSE,idvars=NULL,logs=NULL,ts=NULL,
       cs=NULL,casepri=NULL,empri=NULL,tolerance=0.0001,polytime=NULL,
       startvals=0,lags=NULL,leads=NULL,intercs=FALSE,archive=TRUE,
       sqrts=NULL,lgstc=NULL,noms=NULL,incheck=T,ords=NULL,
       collect=FALSE,outname="outdata",write.out=TRUE,arglist=NULL,
       keep.data=TRUE, priors=NULL)}

\arguments{
  \item{data}{incomplete data, organized into either a data frame 
    or a matrix.}
  
  \item{m}{number of imputed datasets to create.}
  
  \item{p2s}{an interger value taking either 0 for no screen output, 1
  for normal screen printing of iteration numbers, and 2 for detailed
  screen output.  See "Details" for specifics on output when p2s=2.}
    
  \item{frontend}{a logical value used internally for the GUI.}
  
  \item{idvars}{a vector of column numbers or column names that 
    indicates identification variables. }
    
  \item{logs}{a vector of column numbers or column names that refer 
    to variables that require log-linear transformation. }
    
  \item{ts}{number or name indicating column of the time series variable.}
  
  \item{cs}{number or name indicating column of the cross section variable.}
  
  \item{casepri}{indicator matrix of size \emph{kxk} (where \emph{k} is 
    the number of cases) for the degree of similarity between two cases.  
    For example, the [2,3] entry would indicate how similar cases 2 and 3 
    were.  The indicators can be 0, 1, 2, or 3.  Values should only appear
    in the upper triangle, as values in the lower triangle are ignored.}
    
  \item{priors}{a four or five column matrix containing the priors for
    either individual missing observations or variable-wide missing
    values.  See "Details" for more information.}

    
  \item{empri}{number indicating level of the empirical (or ridge) prior.
    This prior shinks the covariances of the data, but keeps the means
    and variances the same for problems of high missingness, small N's or
    large correlations among the variables.  Should be kept small; a 
    reasonable upper bound is around 10\% of the rows of the data.}
  
  \item{tolerance}{convergence requirement.}
  
  \item{polytime}{integet between 0 and 3 indicating what power of 
    polynomial should be included in the imputation model to account for
    the effects of time.  A setting of 0 would indicate no time effects, 
    1 would indicate linear time effects, 2 would indicate squared effects,
    and 3 would indicate cubic time effects. }
  
  \item{lags}{a vector of numbers or names indicating columns in the data
    that should have their lags included in the imputation model.}
  
  \item{leads}{a vector of numbers or names indicating columns in the data
    that should have their leads (future values) included in the imputation 
    model.}
    
  \item{intercs}{a logical variable indicating if the time effects of 
    \code{polytime} should vary across the cross-section.}
  
  \item{archive}{a logical variable indicating whether a replication archive
    should be saved.  This archive includes all of the settings, the results
    of each imputation and some information about the convergence.  The
    output will be saved as \code{'amarchive.R'} in your working directory.}
    
  \item{sqrts}{a vector of numbers or names indicating columns in the data
    that should be transformed by a sqaure root function.  Data in this
    column cannot be less than zero.}
  
  \item{lgstc}{a vector of numbers or names indicating columns in the data
    that should be transformed by a logistic function for proportional data.
    Data in this column must be between 0 and 1.}
      
  \item{noms}{a vector of numbers or names indicating columns in the data
    that are nominal variables.}

  \item{ords}{a vector of numbers or names indicating columns in the
    data that are ordinal variables.}

  \item{collect}{a logical value indicating whether or not garbage
    collection should occur during the imputations.  Only set this to
    \code{TRUE} if you are experiencing memory issues as it can
    significantly slow down the imputation process.}

  \item{incheck}{a logical indicating whether or not the inputs to the
    function should be checked before running \code{amelia}.  This should
    only be set to \code{FALSE} if you are extremely confident that your
    settings are non-problematic and you are trying to save computational 
    time.}

  \item{outname}{a string indicating the prefix of the file to which
    Amelia will write the imputed datasets.  You can also specify a path
    in front of the prefix if you do not wish your items stored in the
    working directory.  The files will be written as .csv files.}

  \item{write.out}{a logical value indicating whether or not you wish to
    have Amelia write your imputed datasets as comma-seperated value files.
    If \code{TRUE}, Amelia will use the \code{outname} arugment as the
    file prefix.}

  \item{arglist}{an output list from the \code{amelia} function or from
    a saved session from AmeliaView.  Values from this list take precendent
    over any individually set arugments.   See the Amelia manual for more
    information.}

  \item{keep.data}{a logical value indicating whether or not to keep the
    imputed datasets after each imputation.  Useful if the datasets are
    large and you wish to avoid keeping them in memory after they have been
    written to a file.}

  \item{startvals}{starting values, 0 for the parameter matrix from
    listwise deletion, 1 for an identity matrix.}

}

\details{ Multiple imputation is a method for analyzing incomplete
  multivariate data.  This function will take an incomplete dataset in
  either data frame or matrix form and return \code{m} imputed datatsets
  with no missing values. The algorithm used will bootstrap a sample dataset
  with the same dimensions as the original and then impute the missing
  values of sample.  It repeats this process \code{m} times to produces
  the output data.
  
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
 
\value{A list containing the imputed datasets in objects 1 through \code{m}.
  Thus, you can refer to any of the datasets by referencing 
  \code{output[[i]]}, where \code{i} is the number of the dataset you wish
  to reference.
  
  These datasets will be returned in the same format which you passed them.
  For example, if you passed a data frame to \code{amelia} you will have
  \code{m} data frames in the output list.  If you passed a matrix, you 
  will have \code{m} matrices in the output.  
  
  Other objects in the list:
  
  \item{code}{return code for the function.  0 indicates a successful run
    of Amelia.  Other codes refer to various problems in data or settings.
    Please refer to the error message and the Amelia manual for help with
    errors.}
  
  \item{message}{error message.  Only appears if return code is not 0.}

  \item{amelia.args}{list of the arguments used in the imputation along
    with a few diagnostics on each imputation.}
  \item{thetas}{a matrix of the output parameter matrices used to
    generate the imputed datasets.}
}

\keyword{models}
