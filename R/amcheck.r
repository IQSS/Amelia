## amcheck.r
## Function for checking for errors in coding
## of the data or input vectors
##
## 21/10/05 - now converts variables names to column numbers, stops if variable doesn't exist; returns codes and messages, doesn't stop execution
## 04/05/06 mb - moved parameter vs. obs check to prep, checks outname
## 10/07/06 mb - fixed handling of variance checks with no fully observed rows.
## 17/07/06 mb - stops if variable only has one observed value.
## 02/08/06 mb - fixed handling of character variables.
## 25/09/06 mb - fixed handling of errors in output writing.
## 13/12/06 mb - removed dropping of extra priors, added new priors
## 15/12/06 mb - fixed problem of nrow(priors)==5



amcheck <- function(data,m=5,p2s=1,frontend=FALSE,idvars=NULL,logs=NULL,
                        ts=NULL,cs=NULL,casepri=NULL,means=NULL,sds=NULL,
                        mins=NULL,maxs=NULL,conf=NULL,empri=NULL,
                        tolerance=0.0001,polytime=NULL,startvals=0,lags=NULL,
                        leads=NULL,intercs=FALSE,archive=TRUE,sqrts=NULL,
                        lgstc=NULL,noms=NULL,incheck=T,ords=NULL,collect=FALSE,
                        outname="outdata",write.out=TRUE,arglist=NULL,
                        keep.data=TRUE, priors=NULL,bounds=NULL,
                        max.resample=1000) {

  #Checks for errors in list variables
  listcheck<-function(vars,optname) {
    if (identical(vars,NULL))
      return(0)
    if (mode(vars) == "character") { 
      if (any(is.na(match(vars,colnames(data))))) {
        mess<-paste("The following variables are refered to in the",
                    optname,"argument, but don't are not columns in the data:",
                    vars[is.na(match(vars,colnames(data)))])
          return(list(1,mess))
        }
       return(0)
    }
    if (any(vars>AMp,vars<0,vars%%1!=0)) {
      mess<-paste(optname," is out of the range of \n",
                  "possible column numbers or is not an integer.")
      return(list(2,mess))
    }
    return(0) 
  }
  
  #Checks for errors in logical variables
  logiccheck<-function(opt,optname) {
    if (!identical(opt,NULL)) {
      if (length(opt) > 1) {
        mess<-paste("The",optname,"setting is longer than one logical.")
        return(list(1,mess))
      }
      if (mode(opt) != "logical") {
        mess<-paste("The",optname,"setting is not a logical (TRUE/FALSE) value.")
        return(list(2,mess))
      }
    } else {
      mess<-paste("The",optname,"setting cannot be NULL.  Please change to TRUE/FALSE.")
      return(list(3,mess))
    }
    return(0)
  }
  
  #Checks for errors in priors variables
  priorcheck<-function(opt,optname) {
    if (!identical(opt,NULL)) {
      if (!is.matrix(opt)) {
        mess<-paste("The", optname,"matrix is not a matrix.\n")
        return(list(1,mess))
      }
      if (is.character(opt)) {
        mess<-paste("The", optname,"matrix is a character matrix.\n",  
                          "Please change it to a numeric matrix.")
        return(list(2,mess))
      }
      if (any(dim(opt)!=dim(data))) {
        mess<-paste("The", optname,"matrices must have the same dimensions\n",
                        "as the data.")
        return(list(3,mess))
      }
    }
    return(0)
  }

  error.code <- 1




  #Error Code: 3
  #Arguments point to variables that do not exist.
  if (inherits(try(get("data"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the data argument doesn't exist.")))
  if (inherits(try(get("m"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'm' argument doesn't exist.")))
 
  if (inherits(try(get("idvars"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'idvars' argument doesn't exist.")))
 
  if (inherits(try(get("means"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'means' argument doesn't exist.")))

  if (inherits(try(get("sds"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'sds' argument doesn't exist.")))

  if (inherits(try(get("mins"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'mins' argument doesn't exist.")))

  if (inherits(try(get("maxs"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'maxs' argument doesn't exist.")))
 
  if (inherits(try(get("conf"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'conf' argument doesn't exist.")))

  if (inherits(try(get("empri"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'empri' argument doesn't exist.")))

  if (inherits(try(get("ts"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'ts' argument doesn't exist.")))

  if (inherits(try(get("cs"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'cs' argument doesn't exist.")))

  if (inherits(try(get("tolerance"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'tolerance' argument doesn't exist.")))

  if (inherits(try(get("casepri"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'casepri' argument doesn't exist.")))

  if (inherits(try(get("polytime"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'polytime' argument doesn't exist.")))

  if (inherits(try(get("lags"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'lags' argument doesn't exist.")))

  if (inherits(try(get("leads"),silent=T),"try-error") )
    return(list(code=3,mess=paste("The setting for the 'leads' argument doesn't exist.")))

  if (inherits(try(get("logs"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'logs' argument doesn't exist.")))

  if (inherits(try(get("sqrts"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'sqrts' argument doesn't exist.")))

  if (inherits(try(get("lgstc"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'lgstc' argument doesn't exist.")))
  
  if (inherits(try(get("p2s"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'p2s' argument doesn't exist.")))

  if (inherits(try(get("frontend"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'frontend' argument doesn't exist.")))

  if (inherits(try(get("archive"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'archive' argument doesn't exist.")))

  if (inherits(try(get("intercs"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'intercs' argument doesn't exist.")))

  if (inherits(try(get("noms"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'noms' argument doesn't exist.")))

  if (inherits(try(get("startvals"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'startvals' argument doesn't exist.")))

  if (inherits(try(get("ords"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'ords' argument doesn't exist.")))
  
  if (inherits(try(get("collect"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'collect' argument doesn't exist.")))
    
  if (inherits(try(get("outname"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'outname' argument doesn't exist.")))
    
  if (inherits(try(get("write.out"),silent=T),"try-error"))
    return(list(code=3,mess=paste("The setting for the 'write.out' argument doesn't exist.")))
    
    
  AMn<-nrow(data)
  AMp<-ncol(data)
  subbedout<-c(idvars,cs,ts)
  
  #Error Code: 4
  #Completely missing columns
  if (any(colSums(!is.na(data)) <= 1)) {
    error.code<-4
    error.mess<-paste("The data has a column that is completely missing or only has one \n",
                      "observation.  Remove this column from the data and retry amelia.")
    return(list(code=error.code,mess=error.mess))
  }
  
  #Error codes: 5-6
  #Errors in one of the list variables
  idout<-listcheck(idvars,"One of the 'idvars'")
  if (!identical(idout,0))
    return(list(code=(idout[[1]]+4),mess=idout[[2]]))
    
  lagout<-listcheck(lags,"One of the 'lags'")
  if (!identical(lagout,0)) 
    return(list(code=(lagout[[1]]+4),mess=lagout[[2]]))

  leadout<-listcheck(leads,"One of the 'leads'")
  if (!identical(leadout,0))
    return(list(code=(leadout[[1]]+4),mess=leadout[[2]]))

  logout<-listcheck(logs,"One of the 'logs'")
  if (!identical(logout,0))
    return(list(code=(logout[[1]]+4),mess=logout[[2]]))

  sqout<-listcheck(sqrts,"One of the 'sqrts'")
  if (!identical(sqout,0))
    return(list(code=(sqout[[1]]+4),mess=sqout[[2]]))

  lgout<-listcheck(lgstc,"One of the 'lgstc'")
  if (!identical(lgout,0))
    return(list(code=(lgout[[1]]+4),mess=lgout[[2]]))

  tsout<-listcheck(ts,"The 'ts' variable")
  if (!identical(tsout,0))
    return(list(code=(tsout[[1]]+4),mess=tsout[[2]]))
    
  csout<-listcheck(cs,"The 'cs' variable")
  if (!identical(csout,0))
    return(list(code=(csout[[1]]+4),mess=csout[[2]]))
  
  nomout<-listcheck(noms,"One of the 'noms'")
  if (!identical(nomout,0))
    return(list(code=(nomout[[1]]+4),mess=nomout[[2]]))
  
  ordout<-listcheck(ords,"One of the 'ords'")
  if (!identical(ordout,0))                                  # THIS FORMERLY READ "NOMOUT"
    return(list(code=(ordout[[1]]+4),mess=ordout[[2]]))
  
  # priors errors
  if (!identical(priors,NULL)) {

    # Error code: 7
    # priors isn't a matrix
    if (!is.matrix(priors)) {
      error.code <- 7
      error.mess <- "The priors argument is not a matrix."
      return(list(code=error.code, mess=error.mess))
    }

    # Error code: 8
    # priors is not numeric
    if (!is.numeric(priors)) {
      error.code <- 7
      error.mess <- paste("The priors matrix is non-numeric.  It should\n",
                          "only have numeric values.")
      return(list(code=error.code, mess=error.mess))

    }

    # Error code: 47
    # priors matrix has the wrong dimensions
    if (ncol(priors) != 4 & ncol(priors) != 5) {
      error.code <- 47
      error.mess <- paste("The priors matrix has the wrong numberof columns.\n",
                          "It should have either 4 or 5 columns.",)
      return(list(code=error.code, mess=error.mess))
    }

    if (nrow(priors) > nrow(data)*ncol(data)) {
      error.code <- 47
      error.mess <- "There are more priors than there are observations."
      return(list(code=error.code, mess=error.mess))
    }

    
    # Error code: 48
    # NAs in priors matrix
    if (any(is.na(priors))) {
      error.code <- 48
      error.mess <- "There are missing values in the priors matrix."
      return(list(code=error.code, mess=error.mess))
    }  

    # Error code: 49
    # multiple priors set
    if (any(duplicated(priors[,1:2]))) {
      error.code <- 49
      error.mess <- "Multiple priors set on one observation or variable."
      return(list(code=error.code,mess=error.mess))
    }
    
    prior.cols <- priors[,2] %in% c(1:ncol(data))
    prior.rows <- priors[,1] %in% c(0:nrow(data))

    # Error code: 9
    # priors set for cells that aren't in the data
    if (sum(c(!prior.cols,!prior.rows)) != 0) { 
      error.code <- 9
      error.mess <- "There are priors set on cells that don't exist."
      return(list(code=error.code,mess=error.mess))
    }

    # Error code: 12
    # confidences have to be in 0-1
    if (ncol(priors) == 5) {
      if (any(priors[,5] <= 0) || any(priors[,5] >= 1)) {
        error.code<-12
        error.mess<-paste("The priors confidences matrix has values that are less \n",
                          "than or equal to 0 or greater than or equal to 1.")
        return(list(code=error.code,mess=error.mess))
      }
    }
        
  }
  #Error code: 10
  #Square roots with negative values
  if (!identical(sqrts,NULL)) {
    if (any(na.omit(data[,sqrts]) < 0)) {
      error.code<-10
      error.mess<-paste("The square root transformation cannot be used on \n",
                        "variables with negative values.")
      return(list(code=error.code,mess=error.mess))
    }
  }

  
  #warning message
  #logs with negative values
  if (!identical(logs,NULL)) {
    if (any(na.omit(data[,logs]) < 0)) { 
      warning(paste("The log transformation is being used on \n",
                    "variables with negative values. The values \n",
                    "will be shifted up by 1 plus the minimum value \n",
                    "of that variable."))
    }
  }
  
  #Error code: 11
  #0-1 Bounds on logistic transformations
  if (!identical(lgstc,NULL)) {
    if (any(na.omit(data[,lgstc]) <= 0,na.omit(data[,lgstc]>=1))) {
      error.code<-11
      error.mess<-paste("The logistic transformation cannot be used on \n",
                        "variables with values out of the 0-1 range.")
      return(list(code=error.code,mess=error.mess))
    }
    
  }
  
  #Error code: 12
  #Confidence Intervals for priors bounded to 0-1
  
#  if (!identical(conf,NULL)) {
#    if (any(conf <= 0,conf>=1,na.rm=T)) {
#      error.code<-12
#      error.mess<-paste("The priors confidences matrix has values that are less \n",
#                        "than or equal to 0 or greater than or equal to 1.")
#      return(list(code=error.code,mess=error.mess))
#    }
#  }

  #Error code: 13
  #Can't set all variables to 'idvar'
  if (!identical(idvars,NULL)) {
    if ((AMp-1) <= length(idvars)) {
      error.code<-13
      error.mess<-paste("You cannot set all variables (or all but one) as ID variables.")
      return(list(code=error.code,mess=error.mess))
    }
  }


  #Error code: 14
  #ts canonot equal cs
  if (!identical(ts,NULL) && !identical(cs,NULL)) {
    if (ts==cs) {
      error.code<-14
      error.mess<-paste("Time series and cross-sectional variables cannot be the same.")
      return(list(code=error.code,mess=error.mess))
    }
  }
  #Error code: 15
  #TS is more than one integer
  if (!identical(ts,NULL)) {
    if (length(ts) > 1) {
      error.code<-15
      error.mess<-paste("The time series variable option is longer than one integer.")
      return(list(code=error.code,mess=error.mess))
    }
  }
  #Error code: 16
  #CS is more than one integer  
  if (!identical(cs,NULL)) {
    if (length(cs) > 1) {
      error.code<-16
      error.mess<-paste("The cross section variable option is longer than one integer.")
      return(list(code=error.code,mess=error.mess))
    }
  }

  if (!identical(casepri,NULL)) {
    #Error code: 17
    #Case prior must be in a matrix
    if (!is.matrix(casepri)) {
      error.code<-17
      error.mess<-paste("The case priors should be in a martix form.")
      return(list(code=error.code,mess=error.mess))
    }
    #Error code: 18
    #CS must be specified with case priors
    if (identical(cs,NULL)) {
      error.code<-18
      error.mess<-paste("The cross-sectional variable must be set in order to use case priors.")
      return(list(code=error.code,mess=error.mess))
    }
    #Error code: 19
    #Case priors have the wrong dimensions
    if (sum(dim(casepri) == c(length(unique(data[,cs])),length(unique(data[,cs])))) != 2) {
      error.code<-19
      error.mess<-paste("The case priors have the wrong dimensions.  It should \n", 
                           "have rows and columns equal to the number of cases.")
      return(list(code=error.code,mess=error.mess))
    }
    #Error code: 20
    #Case prior values are out of bounds
    if (all(casepri != 0,casepri!=1,casepri!=2,casepri!=3)) {
      error.code<-20
      error.mess<-paste("The case priors can only have values 0, 1, 2, or 3.")
      return(list(code=error.code,mess=error.mess))
    } 
  }

  #check polynomials
  if (!identical(polytime,NULL)) {
    #Error code: 21
    #Polynomials of time are longer than one integer
    if (length(polytime) > 1) {
      error.code<-21
      error.mess<-paste("The polynomials of time setting is greater than one integer.")
      return(list(code=error.code,mess=error.mess))
    }
    if (!is.numeric(polytime)) {
      error.code<-22
      error.mess<-paste("The setting for polytime is not a number.")
      return(list(code=error.code,mess=error.mess))
    }
    if ((polytime %% 1) != 0) {
      error.code<-23
      error.mess<-paste("The number of polynomial terms to include for time (polytime) must be an integer.")
      return(list(code=error.code,mess=error.mess))
    }
    if (any(polytime > 3,polytime < 0)) {
      error.code<-24
      error.mess<-paste("The number of polynomial terms to include must be between 1 and 3.")
      return(list(code=error.code,mess=error.mess))
    }
    if (identical(ts,NULL)) {
      error.code<-25
      error.mess<-paste("You have set polynomials of time without setting the time series variable.")
      return(list(code=error.code,mess=error.mess))
    }
    if (all(!intercs,identical(polytime,0))) {
      warning(paste("You've set the polynomials of time to zero with no interaction with \n",
                    "the cross-sectional variable.  This has no effect on the imputation."))
    }
  }
  
  #checks for intercs

  
  if (identical(intercs,TRUE)) {
    if (identical(cs,NULL)) {
      error.code<-27
      error.mess<-paste("You have indicated an interaction with the cross section \n",
                        "without setting the cross section variable.")
      return(list(code=error.code,mess=error.mess))
    }
    if (length(unique(data[,cs])) > (1/3)*(AMn)) {
      error.code<-28
      error.mess<-paste("There are too many cross-sections in the data to use an \n",
                        "interaction between polynomial of time and the cross-section.")
      return(list(code=error.code,mess=error.mess))
    }

  }
  
  #Error codes: 29-31
  #logical variable errors
  interout<-logiccheck(intercs,"cross section interaction")
  if (!identical(interout,0))
    return(list(code=(28+interout[[1]]),mess=interout[[2]]))

  #p2sout<-logiccheck(p2s,"print to screen")
  #if (!identical(p2sout,0))
  #  return(list(code=(p2sout[[1]]+28),mess=p2sout[[2]]))
  
  frout<-logiccheck(frontend,"frontend")
  if (!identical(frout,0))
    return(list(code=(frout[[1]]+28),mess=frout[[2]]))
  
  archout<-logiccheck(archive,"archive")
  if (!identical(archout,0))
    return(list(code=(archout[[1]]+28),mess=archout[[2]]))
  
  collout<-logiccheck(collect,"archive")
  if (!identical(collout,0))
    return(list(code=(collout[[1]]+28),mess=collout[[2]]))
  
  writeout<-logiccheck(write.out,"archive")
  if (!identical(writeout,0))
    return(list(code=(writeout[[1]]+28),mess=writeout[[2]]))  
  
  #Error code: 32
  #Transformations must be mutually exclusive 
  if (length(unique(c(logs,sqrts,lgstc,noms,ords,idvars))) != length(c(logs,sqrts,lgstc,noms,ords,idvars))) {
    error.code<-32
    error.mess<-paste("The options for transfomations are not mutually exclusive.  One \n",
                "variable can only be assigned one transformation.  You have the \n",
                "same variable designated for two transformations.")
    return(list(code=error.code,mess=error.mess))
  }
  
  #Error code: 33
  #ts/cs variables can't be transformed
  if (any(unique(c(logs,sqrts,lgstc,noms,ords,idvars)) == ts,unique(c(logs,sqrts,lgstc,noms,ords,idvars)) == cs)) {
    error.code<-33
    error.mess<-paste("The time series and cross sectional variables cannot be transformed.")
    return(list(code=error.code,mess=error.mess))
  }
  
  

 
  #Error code: 35
  #tolerance must be greater than zero
  if (tolerance <= 0) {
    error.code<-35
    error.mess<-paste("The tolerance option must be greater than zero.")
    return(list(code=error.code,mess=error.mess))
  }

  #check nominals
  if (!identical(noms,NULL)) {
    
    for (i in noms) {
      #Error code: 36
      #too many levels on noms
      if (length(unique(data[,i])) > (1/3)*(AMn)) {
        error.code<-36
        error.mess<-paste("The number of categories in your variables set in noms is \n",
                          "greater than one-third the number of observations.  Check \n",
                          "that noms is set correctly.")
        return(list(code=error.code,mess=error.mess))
      }
      
      if (length(unique(data[,i])) > 10)
        warning("\n\nThe number of catagories in one of the variables marked nominal has greater than 10 categories. Check nominal specification.\n\n")
      


      if (all(i==cs,intercs==T)) {
        noms<-noms[noms!=i]
        warning("The cross sectional variable was set as a nominal variable.  Its nominal status has been dropped.")
      }  
    }
  }
  
  if (is.null(c(noms,ords,idvars,cs)))
    fact<-c(1:AMp)
  else
    fact<--c(noms,ords,idvars,cs)
  #Error code: 37
  #factors out of the noms,ids,ords,cs
  if (any(sapply(data[,fact],class)=="factor")) {
    error.code<-37
    error.mess<-paste("You have a \"factor\" variable in the data.  You may \n",
                      "have wanted to set this as a ID variable to remove it \n",
                      "from the imputation model or as an ordinal or nominal \n", 
                      "variable to be imputed.  Please set it as either and \n",
                      "try again.") 
    return(list(code=error.code,mess=error.mess))
  }
  if (is.null(c(cs,idvars,noms)))
    idcheck<-c(1:AMp)
  else
    idcheck<--c(cs,idvars,noms)
  if (any(sapply(data[,idcheck],class)=="character")) {
    error.code<-38
    error.mess<-paste("You have a \"character\" variable in the data.  You may \n",
                      "have wanted to set this as a ID variable,
nominal\n",
                      "or the cross sectional variable.  Please either remove it from \n",
                      "the data or set it as an ID variable.") 
    return(list(code=error.code,mess=error.mess))
  }
  
  #Error code: 39
  #No missing observation
  if (!any(is.na(data))) {
    error.code<-39
    error.mess<-paste("Your data has no missing values.  Make sure the code for \n",
                      "missing data is set to the code for R, which is NA.")
    return(list(code=error.code,mess=error.mess))
  }
  
  #Error code: 40
  #lags require ts
  if (!identical(lags,NULL)) {
    if (identical(ts,NULL)) {
      error.code<-40
      error.mess<-paste("You need to specify the time variable in order to create lags.")
      return(list(code=error.code,mess=error.mess))
    }
  }
  
  #Error code: 41
  #leads require ts
  if (!identical(leads,NULL)) {
    if (identical(ts,NULL)) {
      error.code<-41
      error.mess<-paste("You need to specify the time variable in order to create leads.")
      return(list(code=error.code,mess=error.mess))
    }
  }
  
  
  #Error code: 42
  #Only 1 column of data
  if (AMp==1) {
    error.code<-42
    error.mess<-paste("There is only 1 column of data. Cannot impute.")
    return(list(code=error.code,mess=error.mess))
  }  
  

  #Error code: 43
  #Variable that doesn't vary
  if (is.data.frame(data)) {
    
      if (any(sapply(data[,idcheck,drop=FALSE],var,na.rm=T)==0)) {
        error.code<-43
        error.mess<-paste("You have a variable in your dataset that does not vary.  Please remove this variable.")
        return(list(code=error.code,mess=error.mess))
      }     
  } else {
    if (nrow(na.omit(data)) > 1) {
      if (any(diag(var(data[,idcheck],na.rm=T))==0)) {
        error.code<-43
        error.mess<-paste("You have a variable in your dataset that does not vary.  Please remove this variable.")
        return(list(code=error.code,mess=error.mess))
      }
    } else {
      for (i in 1:ncol(data[,idcheck])) {
        if (var(data[,i],na.rm=T) == 0) {
          error.code<-43
          error.mess<-paste("You have a variable in your dataset that does not vary.  Please remove this variable.")
          return(list(code=error.code,mess=error.mess))
        }
      }
    }
  }
    
  #checks for ordinals
  if (!is.null(ords)) {
    for (i in ords) {
      #Error code: 44
      # Ordinal variable with non-integers (factors work by design, and they're
      # harder to check
      if (!is.factor(data[,i])) {
        if (any(unique(na.omit(data[,i])) %% 1 != 0 )) {
          error.code<-44
          error.mess<-paste("You have designated a variable as ordinal when it has non-integer values.")
          return(list(code=error.code,mess=error.mess))
        }
      }
    }    
  }

 
  
  #checks for outname
  if (write.out==T) {
    if (!is.character(outname)) {
      outname<-"outdata"
      warning("The output filename (outname) was not a character.  It has been set it 
its default 'outdata' in the working directory.")
    }
    #Error code: 45
    #output file errors
    outtest<-try(write.csv("test",file=paste(outname,"1.csv",sep="")),silent=T)
    if (inherits(outtest,"try-error")) {
      error.code<-45
      error.mess<-paste("R cannot write to the outname you have specified.  Please 
check","that the directory exists and that you have permission to write.",sep="\n")
      return(list(code=error.code,mess=error.mess))
    }
    tmpdir<- strsplit(paste(outname,"1.csv",sep=""),.Platform$file.sep)
    am.dir <- tmpdir[[1]][1]
    if (length(tmpdir[[1]]) > 1)
      for (i in 2:(length(tmpdir[[1]])))
        am.dir <- file.path(am.dir, tmpdir[[1]][i])
    file.remove(am.dir)
  }
      

#  if (xor(!identical(means,NULL),!identical(sds,NULL))) {
#    means<-NULL
#    sds<-NULL
#    warning("Both the means and the SDs have to be set in order to use observational priors.  The priors have been removed from the analysis.")
#  }
#  if (sum(!identical(mins,NULL),!identical(maxs,NULL),!identical(conf,NULL)) != 3 &&
#        sum(!identical(mins,NULL),!identical(maxs,NULL),!identical(conf,NULL)) != 0) {
#    mins<-NULL
#    maxs<-NULL
#    conf<-NULL
#    warning("Not all of the range parameters were set for the observational priors.  They have been removed.")
#  }
  
    #checks of m
  if (!is.numeric(m)) {
    m<-5
    warning("The number of imputations ('m') was a non-numeric.  The value was changed to the default.")
  }
  if ((m %% 1) != 0) {
    m<-5
    warning("The number of imputation ('m') was not an integer.  The value was changed to the default (5).")
  }
  if (m<=0) {
    m<-5
    warning("The number of imputations ('m') must be greater than 0.  The value was changed to the default (5).")
  }


  # checks for bounds
  if (!identical(bounds,NULL)) {
    b.size <- is.matrix(bounds) && ncol(bounds)==3 && nrow(bounds) > 0
    b.cols <- sum(bounds[,1] %in% c(1:AMp)) == nrow(bounds)
    maxint <- max.resample > 0 && (max.resample %% 1)==0
    

    # Error 50:
    # wrong sized bounds matrix
    if (!b.size) {
      error.code<-50
      error.mess<-paste("The bounds argument is a three-column matrix.")
      return(list(code=error.code,mess=error.mess))
    }

    # Error 51:
    # nonexistant columns in bounds.
    if (!b.cols) {
      error.code<-51
      error.mess<-paste("One of the bounds is on a non-existant column.")
      return(list(code=error.code,mess=error.mess))
    }

    # Error 52:
    # max.resample needs to be positive integer.
    if (!maxint) {
      error.code<-52
      error.mess<-paste("The max.resample argument needs to be a positive integer.")
      return(list(code=error.code,mess=error.mess))
    }      
  }

  
  return(list(m=m,outname=outname,priors=priors))
}
