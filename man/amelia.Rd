\name{amelia}
\alias{amelia}

\title{AMELIA: Mutliple Imputation of Incomplete Multivariate Data}
\description{
  Runs the bootstrap EM algorithm on incomplete data and creates 
  imputed datasets.  
}
\usage{
amelia(data,m=5,p2s=1,frontend=FALSE,idvars=NULL,logs=NULL,ts=NULL,cs=NULL,casepri=NULL,means=NULL,
                  sds=NULL,mins=NULL,maxs=NULL,conf=NULL,empri=NULL,tolerance=0.00001,polytime=NULL,startvals=0,
                  lags=NULL, leads=NULL, intercs=FALSE,archive=TRUE,sqrts=NULL,lgstc=NULL,noms=NULL,incheck=T,
                  ords=NULL,collect=FALSE,outname="outdata",write.out=TRUE)
}

\arguments{
  \item{data}{incomplete data, organized into either a data frame 
    or a matrix.}
  
  \item{m}{number of imputed datasets to create.}
  
  \item{p2s}{a logical value indicating if progress should be printed 
    to the screen.}
    
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
    
  \item{means}{a matrix with the same dimensions as the data where each 
    cell contains a prior belief about the mean value of that cell in the 
    data.  For example, the values in \code{means[2,3]} would be your prior 
    belief about the mean of data[2,3].  Cell should be \code{NA} if no 
    prior exists. If \code{means} is set, \code{sds} must be set as well.}
    
  \item{sds}{a matrix with the same dimensions as the data where each 
    cell contains a prior belief about the stardard deviation around the 
    prior mean of that cell in the data.  For example, the values in 
    \code{sds[2,3]} would be your prior belief about the \code{means[2,3]} 
    of \code{data[2,3]}.  Cell should be \code{NA} if no prior exists. If 
    \code{sds} is set, \code{means} must be set as well.}
    
  \item{mins}{a matrix with the same dimensions as the data, used for
    range priors.  A cell in this matrix contains a prior belief about a
    minimum value for that cell in the data.  For example, the values in 
    \code{mins[2,3]} would be your prior belief about the minimum of 
    \code{data[2,3]}.  If \code{mins} is set, \code{maxs} and \code{conf}
    must be set as well.}
    
  \item{maxs}{a matrix with the same dimensions as the data, used for
    range priors.  A cell in this matrix contains a prior belief about a
    maximum value for that cell in the data.  For example, the values in 
    \code{maxs[2,3]} would be your prior belief about the maximum of 
    \code{data[2,3]}.  If \code{maxs} is set, \code{mins} and \code{conf}
    must be set as well.}
    
  \item{conf}{a matrix with the same dimensions as the data, used for
    range priors.  A cell in this matrix contains your prior confidence
    about the range prior for that cell in the data.  For example, the 
    values in \code{maxs[2,3]} would be your prior belief about the maximum 
    of \code{data[2,3]}.  If \code{maxs} is set, \code{mins} and \code{conf}
    must be set as well.}
    
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
    
  \item{incheck}{a logical indicating whether or not the inputs to the
    function should be checked before running \code{amelia}.  This should
    only be set to \code{FALSE} if you are extremely confident that your
    settings are non-problematic and you are trying to save computational 
    time.}
}
\details{Multiple imputation is a method for analyzing incomplete 
  multivariate data.  This function will take an incomplete dataset in
  either data frame or matrix form and return \code{m} imputed datatsets
  with no missing values. The algorithm used will bootstrap a sample dataset
  with the same dimensions as the original and then impute the missing
  values of sample.  It repeats this process \code{m} times to produces
  the output data.
  
  You can give Amelia your informational priors about the data in a number
  of ways.  If you have information about the way in which two cases 
  interact, you case use the \code{casepri} setting.  If you have a prior
  belief about the values of a single observation or group of observations,
  you can set them in one of two ways.  First, you can set a mean and 
  standard deviation for the observation (this assumes a normal distribution)
  by setting the \code{means} and \code{sds} matrices in the same location
  as the observation.  For example, if you had a thought that the 
  observation located at [50,7] in your data had a mean of 0 and a standard
  deviation of 1, you would pass to \code{amelia} a \code{means} matrix
  with a 0 in the [50,7] location and a seperate \code{sds} matrix with
  a 1 in the [50,7] location.  Observations for which no informational
  prior exist, should be left as \code{NA} in both of these matrices.
  
  Second, you can set a range of values that you are confident the 
  observation will fall in.  In a similar manner as above, you place the
  minimum and maximum values in the location of the observation in the
  \code{mins} and \code{maxs}, respectively.  We also require that you 
  specify how confident you are about the range as a value between 0 and 1.
  This goes in the \code{conf} matrix in the same location as observation.
  
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
}

\keyword{models}
