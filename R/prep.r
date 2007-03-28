##
## prep.r
##
## Various routines for transforming the original data to the imputation model, 
## and reverting back to the format of the original data
## 28/04/06 mb functions extracted from emb.r to prep.r
## 29/04/06 jh revised unsubset for memory issues
## 04/05/06 mb moved parameter vs. observation check to the end of prep. 
## 18/05/06 mb 'ords' unsubset fixed to have range of original values
## 15/06/06 jh revised "generatepriors"
## 26/06/06 mb fixed archive to work with session loading.
## 27/06/06 mb amelia.prep accepts, checks, and processes 'arglist'
## 27/07/06 mb amsubset changes from dataframe to matrix after subsetting to 
##             avoid eating it on strings.
## 02/08/06 mb frame.to.matrix now converts chars to either factors (cs,noms) 
##             or junk (idvars). added to subset/unsubset (ignore last update).
## 02/08/06 mb fixed bug where polytime=0 would make for odd behaviour/crashing
## 11/09/06 mb fixed bug in unsubset: 'index' too long to subset w/o 'which'
## 18/10/06 mb incorporated confidence levels into generating priors
## 20/10/06 mb new format for priors
## 13/12/06 mb indiv. obs priors get priority in generatepriors
## 28/03/07 jh added empri to prepped$archv, modified construction of timevars

nametonumber<-function(x,ts,cs,idvars,noms,ords,logs,sqrts,lgstc,lags,leads)
{
  
  listconvert<-function(opt) {
    junk.seq<-1:ncol(x)
    junk.names<-dimnames(x)[[2]]
    for (i in 1:length(opt)) {
      mat<-opt[i]==junk.names
      if (sum(mat) == 0)
        return(NA)
      opt[i]<-junk.seq[mat]
    }

    return(as.numeric(opt))
  }

  code<-0
  mess<-paste("One of the variable names in the options list does not match a variable name in the data.")

  if (class(ts)=="character")
    ts<-listconvert(ts)
  if (class(cs)=="character")
    cs<-listconvert(cs)
  if (class(idvars)=="character")
    idvars<-listconvert(idvars)
  if (class(noms)=="character")
    noms<-listconvert(noms)
  if (class(ords)=="character")
    ords<-listconvert(ords)
  if (class(logs)=="character")
    logs<-listconvert(logs)
  if (class(sqrts)=="character")
    sqrts<-listconvert(sqrts)
  if (class(lgstc)=="character")
    lgstc<-listconvert(lgstc)
  if (class(lags)=="character")
    lags<-listconvert(lags)
  if (class(leads)=="character")
    leads<-listconvert(leads)

  output<-list(code=code,ts=ts,cs=cs,idvars=idvars,noms=noms,
               ords=ords,logs=logs,sqrts=sqrts,lgstc=lgstc,
               lags=lags,leads=leads,mess=mess)
  
  if (any(is.na(output))) 
      output$code<-1
  
  return(output)
}

## convert.priors - convert 4/5-column priors to matrix of priors
##   priors:  4/5 column priors matrix
##   nrow:    rows of the data matrix
##   ncol:    cols of the data matrix
##
##  output: a list of either 2 (in the 4 col case) or 3 (in the 5 col
##          case) of prior matrices.
#convert.priors <- fucntion(priors, nrow, ncol) {
#  if (!is.matrix(priors))
#    stop("argument 'priors' is not a matrix")
#  if (ncol(priors) != 4 || ncol(priors) != 5)
#    stop("priors matrix has the wrong number of columns")



  
#  if (ncol(priors) == 4) {
#    #generate output priors matrix, the size of the data
#    out.means <- matrix(NA, nrow = nrow, ncol = ncol)
#    out.sds   <- matrix(NA, nrow = nrow, ncol = ncol)

#    # fill in the the matrices
#    for (i in 1:nrow(priors)) {
#      out.means[priors[i,1], priors[i,2]] <- priors[i,3]
#        out.sds[priors[i,1], priors[i,2]] <- priors[i,4]
#    }

#    return(list(means = out.means, sds = out.sds))
#  }

  
#  if (ncol(priors) == 5) {
#    out.mins  <- matrix(NA, nrow = nrow, ncol = ncol)
#    out.maxs  <- matrix(NA, nrow = nrow, ncol = ncol)
#    out.conf  <- matrix(NA, nrow = nrow, ncol = ncol)

#    for (i in 1:nrow(priors)) {
#      out.mins[priors[i,1], priors[i,2]] <- priors[i,3]
#      out.maxs[priors[i,1], priors[i,2]] <- priors[i,4]
#      out.conf[priors[i,1], priors[i,2]] <- priors[i,5]
#    }

#    return(list(mins = out.mins, maxs = out.maxs, conf = out.conf))
#  }
#}


## amtransform - Transform variables to assume normality
##   x:      data matrix
##   logs:   variable list of log-linear transformations
##   sqrts:  variable list of square root transformations
##   lgstc:  variable list of logistic transformations
##   xmin:   vector of column minimums
amtransform<-function(x,logs,sqrts,lgstc) {
  
  logs<-unique(logs)
  sqrts<-unique(sqrts)
  lgstc<-unique(lgstc)
  xmin<-c()
  if (!is.null(logs)) {
    for (i in 1:length(logs)) {
      j<-logs[[i]]
      xmin<-c(xmin,min(x[,j],na.rm=T))  #we need mins to avoid creating NAs
      x[,j]<-log(x[,j]-xmin[[i]]+(2*.Machine$double.eps))     #by taking a log of a negative number
    }
  }

  if (!is.null(sqrts))
    for (i in sqrts)
      x[,i]<-sqrt(x[,i])

  if (!is.null(lgstc))
    for (i in lgstc)
      x[,i]<-log(x[,i]/(1-x[,i]))



  return(list(x=x,xmin=xmin))
}

## untransform - Convert imputed variables to original scale
##   x.imp:    imputed data matrix
##   logs:   variable list of log-linear transformations
##   xmins:  vector of column minimums
##   sqrts:  variable list of square root transformations
##   lgstc:  variable list of logistic transformations
untransform<-function(x.imp,logs,xmin,sqrts,lgstc) {
  logs<-unique(logs)
  sqrts<-unique(sqrts)
  lgstc<-unique(lgstc)
  if (!is.null(logs)) {
    for (i in 1:length(logs)) {
      j<-logs[[i]]
      x.imp[,j]<-exp(x.imp[,j])+xmin[[i]]
    }
  }

  if (!is.null(sqrts))
    for (i in sqrts)
      x.imp[,i]<-(x.imp[,i])^2

  if (!is.null(lgstc))
    for (i in lgstc)
      x.imp[,i]<-exp(x.imp[,i])/(1 + exp(x.imp[,i]))

  return(x.imp)
}

frame.to.matrix<-function(x,idvars) {
  char.vars<-which(sapply(x,class)=="character")
  if (length(char.vars) > 0)
    for (i in char.vars)
      if (is.na(match(i,idvars)))
        x[,i]<-as.factor(x[,i])         #changes cs/noms char. vars to factors
      else
        x[,i]<-1                        #junks id char vars.
        
        
  return(data.matrix(x))                #return it as matrix
}
      


## Remove rows and columns from dataset that do not belong
amsubset<-function(x,idvars,p2s,ts,cs,priors=NULL,
                  polytime=NULL,intercs=FALSE,lags=NULL,leads=NULL,noms=NULL) {

  lags   <- unique(lags)
  leads  <- unique(leads)
  noms   <- unique(noms)
  idvars <- unique(idvars)
  index  <- c(1:ncol(x))


  if (!is.null(idvars))
    index<-index[-idvars]

  if (is.data.frame(x))
    x<-frame.to.matrix(x,idvars)


  if (!is.null(lags)) {
    if (!identical(cs,NULL)) {
      tsarg<-list(x[,cs],x[,ts])
    } else {
      tsarg<-list(x[,ts])
    }
    tssort<-do.call("order",tsarg)
    x.sort<-x[tssort,]
    for (i in lags) {
      lagged<-c(NA,x.sort[1:(nrow(x)-1),i])
      #lagged<-c(NA,lagged)
      if (!identical(cs,NULL)) {
        for (i in 2:nrow(x.sort))
          if (x.sort[i,cs]!=x.sort[i-1,cs])
            is.na(lagged)<-i
      }
      x.sort<-cbind(x.sort,lagged)
      x<-cbind(x,1)
      index<-c(index,-.5)  #-.5=lags
    }
    x[tssort,]<-x.sort
  }
  if (!is.null(leads)){
    if (!identical(cs,NULL)) {
      tsarg<-list(x[,cs],x[,ts])
    } else {
      tsarg<-list(x[,ts])
    }
    tssort<-do.call("order",tsarg)
    x.sort<-x[tssort,]
    for (i in lags) {
      lagged<-x.sort[2:nrow(x),i]
      lagged<-c(lagged,NA)
      if (!identical(cs,NA)) {
        for (i in 1:(nrow(x.sort)-1))
          if (x.sort[i,cs]!=x.sort[i+1,cs])
            is.na(lagged)<-i
      }
      x.sort<-cbind(x.sort,lagged)
      x<-cbind(x,1)
      index<-c(index,.5)  #.5=leads
    }
    x[tssort,]<-x.sort
  }
  
  #puts timeseries and crosssection into the id variable to avoid singularity
  if (!is.null(ts)) {
    index<-index[index!=ts]
    idvars<-c(idvars,ts)
  }
  if (!is.null(cs)) {
    index<-index[index!=cs]
    idvars<-c(idvars,cs)
  }
  
  #nominals
  if (!is.null(noms)) {
    for (i in noms) {
      values<-unique(na.omit(x[,i]))
      newx<-matrix(0,nrow=nrow(x),ncol=length(values)-1)
      index<-index[index!=i]
      for (j in 2:length(values)) {
        newx[,j-1]<-ifelse(x[,i] == values[j],1,0)
        index<-c(index,-i) 
      }
      x<-cbind(x,newx)
      idvars<-c(idvars,i)

    }
  }



  if (!identical(polytime,NULL)){
    time<-x[,ts]
    polynomials<-cbind(1,time,time^2,time^3)
    polynomials<-polynomials[,1:(polytime+1) ,drop=FALSE]
    cstypes<-unique(x[,cs])
    timevars<-matrix(0,nrow(x),1)
    if (intercs){
      for (i in cstypes){
        dummy<-as.numeric(x[,cs]==i)
        timevars<-cbind(timevars,dummy*polynomials)
        timevars<-timevars[,-1]
      }
# CHECK THE FOLLOWING LINE WS.  [,-1] added      ALSO check that too clever dummy constru
    timevars<-timevars[,-1]
    } else {
      timevars<-cbind(timevars,polynomials)
      timevars<-timevars[,-c(1,2)]  # first column is a holding variable, second is to have fixed effects identified
    }
     x<-cbind(x,timevars)  
     for (i in 1:ncol(as.matrix(timevars)))
       index<-c(index,0)               #0 - timevars
  }

  if (!identical(idvars,NULL))
    x<-x[,-idvars]   


  AMr1<-is.na(x)
  flag<-rowSums(AMr1)==ncol(x)
  if (max(flag)==1){
  blanks<-1:nrow(x)
    blanks<-blanks[flag]
    x<-x[!flag,]
  
    priors[,1] <- priors[,1,drop=FALSE] - colSums(sapply(priors[,1,drop=FALSE],">",blanks))
    
  
  

    if (p2s) cat("Warning: There are observations in the data that are completely missing.","\n",
                 "        These observations will remain unimputed in the final datasets.","\n")
  } else {
    blanks<-NULL
  }
  priors[,2] <- match(priors[,2], index)

return(list(x=x,index=index,idvars=idvars,blanks=blanks,priors=priors))
}

## Replace rows and columns removed in "amsubset"
## Create integer values for nominals and ordinals
unsubset<-function(x.orig,x.imp,blanks,idvars,ts,cs,polytime,intercs,noms,index,ords){

#  if (!identical(polytime,NULL)){
#
#    if (intercs){
#      dimtspoly<- (polytime+1)*length(unique(x.orig[,cs])) - 1
#    } else {
#      dimtspoly<- polytime
#    }
#    x.imp<-x.imp[,1:(ncol(x.imp)-dimtspoly)]
#  }

  if (is.data.frame(x.orig)) {
    oldidvars<-idvars[-match(cs,idvars)]
    x.orig<-frame.to.matrix(x.orig,oldidvars)
  }
  AMr1.orig<-is.na(x.orig)
  if (!identical(c(blanks,idvars),c(NULL,NULL))){
    if (identical(blanks,NULL)) {blanks<- -(1:nrow(x.orig))}
    if (identical(idvars,NULL)) {idvars<- -(1:ncol(x.orig))}
    x.orig[-blanks,-idvars]<-x.imp[,1:ncol(x.orig[,-idvars])]
  } else {
    x.orig <- x.imp[,1:ncol(x.orig)]
  }

  if (!is.null(noms)) {
    for (i in noms) {
      y<-runif(nrow(x.imp))
      dums<-x.imp[,which(index==-i)]
      p<-dums*(dums>0)*(dums<1) + ((dums-1) >=0)
      psub<-rowSums(as.matrix(p))
      psub<-(psub <= 1) + (psub)*(psub > 1)
      p<-p/psub
      pzero<-1-rowSums(as.matrix(p))
      p<-cbind(pzero,p)
      cump<-p%*%ifelse(upper.tri(matrix(0,nrow=ncol(p),ncol=ncol(p)),diag=T),1,0)
      yy<-(y<cump)*(y>cbind(matrix(0,nrow(cump),1),cump[,1:(ncol(cump)-1)]))
      renom<-(yy%*%unique(na.omit(x.orig[,i])))
      x.orig[,i]<-renom
    }
  }

  if (!is.null(ords)) {
    ords <- unique(ords)
    x <- x.imp[,ords] * AMr1.orig[,ords]

############ revision #####################
    minmaxords<-matrix(0,length(ords),2)
    for(jj in 1:length(ords)){
      tempords<-x.orig[AMr1.orig[,ords[jj]]==0 ,ords[jj]]
      minmaxords[jj,1]<-min(tempords)
      minmaxords[jj,2]<-max(tempords)
    }
    minord<-minmaxords[,1]
    maxord<-minmaxords[,2]

############ replaces #####################
#   minord <- apply(ifelse(AMr1.orig[,ords]==1,NA,x.orig[,ords]),2,min,na.rm=T)
#   maxord <- apply(ifelse(AMr1.orig[,ords]==1,NA,x.orig[,ords]),2,max,na.rm=T)


    ordrange <- maxord - minord

    p <- t((t(x)-minord)/ordrange) * AMr1.orig[,ords]
    p <- p*(p>0)*(p<1) + ((p-1)>=0)
    newimp <- matrix(0,nrow(x.orig),length(ords))
    for (k in 1:length(ords)) {
      reordnl <- rbinom(nrow(x.orig),ordrange[k],p[,k])
      newimp[,k] <- reordnl + minord[k] * AMr1.orig[,ords[k]]
    }

############# revision #############################
    for(jj in 1:length(ords)){
      x.orig[AMr1.orig[,ords[jj]]==1, ords[jj]]<-newimp[AMr1.orig[,ords[jj]]==1,jj]
    }                                        # MAYBE CAN REMOVE LOOP

############# replaces #############################
#   x.orig[,ords] <- ifelse(AMr1.orig[,ords]==1,0,x.orig[,ords]) + newimp

  }
  return(x.orig)

}
## Rescale Dataset
scalecenter<-function(x,priors=NULL){

  AMn<-nrow(x)
  ones<-matrix(1,AMn,1)
  meanx<-colMeans(x,na.rm=TRUE)
  stdvx<-apply(x,2,sd,na.rm=TRUE)
  x.ztrans<-(x-(ones %*% meanx))/(ones %*% stdvx)
  if (!identical(priors,NULL)){
    priors[,3]<-(priors[,3]-meanx[priors[,2]])/stdvx[priors[,2]]
    priors[,4]<-priors[,4]/stdvx[priors[,2]]
  }
return(list(x=x.ztrans,mu=meanx,sd=stdvx,priors=priors))
}

unscale<-function(x,mu,sd){
  AMn<-nrow(x)
  ones<-matrix(1,AMn,1)
  x.unscale<-(x * (ones %*% sd)) + (ones %*% mu)
return(x.unscale)
}


## Stack dataset and return vectors for sorting
## NOTE:  THIS ORDERS TIES IN A SLIGHTLY DIFFERENT WAY THAN "stack.g" IN GAUSS AMELIA
amstack<-function(x,colorder=TRUE,priors=NULL){

  AMp<-ncol(x)
  AMr1<-is.na(x)

  if (colorder){                                             #Rearrange Columns
    p.order <- order(colSums(AMr1))
    AMr1<-AMr1[,p.order]
  } else {
    p.order<-1:ncol(x)
  }

  n.order <- do.call("order", as.data.frame(AMr1[,AMp:1]))   #Rearrange Rows

  AMr1<- AMr1[n.order,]     # p.order has already been rearranged
  x<- x[n.order,p.order]    # rearrange rows and columns of dataset
  if (!identical(priors,NULL)){
    priors[,1]<-match(priors[,1],n.order)
    priors[,2]<-match(priors[,2],p.order)
  }

  return(list(x=x,n.order=n.order,p.order=p.order,priors=priors))
}


## Rearrange dataset to original ordering of rows and columns
amunstack<-function(x,n.order,p.order){
  x.unstacked<-matrix(0,nrow=nrow(x),ncol=ncol(x))
  x.unstacked[n.order,p.order]<-x
  return(x.unstacked)
}

# This function is in miserable shape.  Need to clean up how lack of priors are dealt with.

generatepriors<-function(AMr1,casepri=NULL,empri=NULL,priors=NULL){

  if (!identical(priors,NULL)) {
    if (ncol(priors) == 5){
      new.priors<-matrix(NA, nrow = nrow(priors), ncol = 4)
      new.priors[,1:2]<-priors[,1:2]           
      new.priors[,3]<-priors[,3] + ((priors[,4] - priors[,3])/2)
      new.priors[,4]<-(priors[,4]-priors[,3])/(2*qnorm(1-(1-priors[,5])/2))
                                        #NOTE: FIX THIS: Currently ignores CONF- ASSUMES CI95
      
    } else {
      new.priors <-priors
    }
    zeros <- which(new.priors[,1]==0)
    if (length(zeros) > 0) {
      varPriors <- new.priors[zeros,2]
      missCells <- which(AMr1[,varPriors,drop=FALSE], arr.ind=TRUE)
      addedPriors <- matrix(NA, nrow=nrow(missCells), ncol=4)
      addedPriors[,1] <- missCells[,1]
      addedPriors[,2] <- varPriors[missCells[,2]]
      addedPriors[,-c(1,2)] <- new.priors[zeros[missCells[,2]],-c(1,2)]
      new.priors <- new.priors[-zeros,,drop=FALSE]

      # find any matches in the rows/cols and remove from addedPriors
      # since we've removed other dups, addedPriors will have the only
      # dups
      new.priors <- rbind(new.priors,addedPriors)
      new.priors <- new.priors[!duplicated(new.priors[,1:2]),]
      return(new.priors)
    }
  }
}

amelia.prep <- function(data,m=5,p2s=1,frontend=FALSE,idvars=NULL,logs=NULL,
                        ts=NULL,cs=NULL,casepri=NULL,empri=NULL,
                        tolerance=0.0001,polytime=NULL,startvals=0,lags=NULL,
                        leads=NULL,intercs=FALSE,archive=TRUE,sqrts=NULL,
                        lgstc=NULL,noms=NULL,incheck=T,ords=NULL,collect=FALSE,
                        outname="outdata",write.out=TRUE,arglist=NULL,
                        keep.data=TRUE, priors=NULL,var=NULL,autopri=0.05) {


  code <- 1
  if (!identical(arglist,NULL)) {
    if (!is.list(arglist) || identical(arglist$amelia.args,NULL)) {
      error.code <- 46
      error.mess <- paste("The argument list you provided is invalid.")
      return(list(code=error.code, message=error.mess))
    }
    m         <- arglist$amelia.args$m
    idvars    <- arglist$amelia.args$idvars
    empri     <- arglist$amelia.args$empri
    ts        <- arglist$amelia.args$ts
    cs        <- arglist$amelia.args$cs
    tolerance <- arglist$amelia.args$tolerance
    casepri   <- arglist$amelia.args$casepri
    polytime  <- arglist$amelia.args$polytime
    lags      <- arglist$amelia.args$lags
    leads     <- arglist$amelia.args$leads
    logs      <- arglist$amelia.args$logs
    sqrts     <- arglist$amelia.args$sqrts
    lgstc     <- arglist$amelia.args$lgstc
    intercs   <- arglist$amelia.args$intercs
    noms      <- arglist$amelia.args$noms
    startvals <- arglist$amelia.args$startvals
    ords      <- arglist$amelia.args$ords
    priors    <- arglist$amelia.args$priors
    autopri   <- arglist$amelia.args$autopri
    empri     <- arglist$amelia.args$empri       #change 1
  }
  
  
  numopts<-nametonumber(x=data,ts=ts,cs=cs,idvars=idvars,noms=noms,ords=ords,
                        logs=logs,sqrts=sqrts,lgstc=lgstc,lags=lags,leads=leads)
  if (numopts$code == 1) {
    return(list(code=44,message=numopts$mess))
  }
  
  if (incheck) {

    checklist<-amcheck(data = data, m = m, idvars = numopts$idvars, priors =
                       priors, empri = empri, ts = numopts$ts, cs = numopts$cs,
                       tolerance = tolerance, casepri = casepri, polytime =
                       polytime, lags = numopts$lags,leads = numopts$leads, logs
                       = numopts$logs, sqrts = numopts$sqrts, lgstc
                       =numopts$lgstc, p2s = p2s, frontend = frontend, archive =
                       archive, intercs = intercs, noms = numopts$noms,
                       startvals = startvals, ords = numopts$ords, collect =
                       collect, outname = outname, write.out = write.out)
    #check.call <- match.call()
    #check.call[[1]] <- as.name("amcheck")
    #checklist <- eval(check.call, parent.frame())
    
    if (!is.null(checklist$code)) {
      return(list(code=checklist$code,message=checklist$mess))
    }
    m <- checklist$m
    priors <- checklist$priors
    outname <- checklist$outname
  }

  priors <- generatepriors(AMr1 = is.na(data) ,casepri = casepri,
                           empri = empri, priors = priors)
  
  
  if (archive) {
    archv<-list(m=m, idvars=numopts$idvars, logs=numopts$logs, ts=numopts$ts, cs=numopts$cs,
                casepri=casepri, empri=empri, tolerance=tolerance,
                polytime=polytime, lags=numopts$lags, leads=numopts$leads,
                intercs=intercs, sqrts=numopts$sqrts, lgstc=numopts$lgstc,
                noms=numopts$noms, ords=numopts$ords, outname=outname,
                priors=priors, autopri=autopri, empri=empri)        #change 2
  } else {
    archv<-NULL
  }
  if (p2s==2) {
    cat("beginning prep functions\n")
    flush.console()
  }

  
  d.trans<-amtransform(data,logs=numopts$logs,sqrts=numopts$sqrts,lgstc=numopts$lgstc)  
  d.subset<-amsubset(d.trans$x,idvars=numopts$idvars,p2s=p2s,ts=numopts$ts,cs=numopts$cs,polytime=polytime,intercs=intercs,noms=numopts$noms,priors=priors)
  d.scaled<-scalecenter(d.subset$x,priors=d.subset$priors)
  d.stacked<-amstack(d.scaled$x,colorder=TRUE,priors=d.scaled$priors)

  if (incheck) {
    realAMp <- ncol(d.stacked$x)
    realAMn <- nrow(d.stacked$x)
  #Error code: 34-35
  #Too few observations to estimate parameters
    if (!identical(empri,NULL)) {
      if (realAMp*2 > realAMn+empri) {
        error.code<-34
        error.mess<-paste("The number of observations in too low to estimate the number of \n",
                        "parameters.  You can either remove some variables, reduce \n",
                        "the order of the time polynomial, or increase the empirical prior.")
        return(list(code=error.code,message=error.mess))
      }
      if (realAMp*4 > realAMn +empri) {
        warning("You have a small number of observations, relative to the number, of variables in the imputation model.  Consider removing some variables, or reducing the order of time polynomials to reduce the number of parameters.")
      }
  
  } else {
    if (realAMp*2 > realAMn) {
      error.code<-34      
        error.mess<-paste("The number of observations is too low to estimate the number of \n",
                        "parameters.  You can either remove some variables, reduce \n",
                        "the order of the time polynomial, or increase the empirical prior.")
      return(list(code=error.code,message=error.mess))
    }
    
    if (realAMp*4 > realAMn) {
        warning("You have a small number of observations, relative to the number, of variables in the imputation model.  Consider removing some variables, or reducing the order of time polynomials to reduce the number of parameters.")
    }
  }
}


  
  return(list(
    x            = d.stacked$x,
    code         = code,
    priors       = d.stacked$priors,
    n.order      = d.stacked$n.order,
    p.order      = d.stacked$p.order,
    scaled.mu    = d.scaled$mu,
    scaled.sd    = d.scaled$sd,
    trans.x      = d.trans$x,
    blanks       = d.subset$blanks,
    idvars       = d.subset$idvars,
    ts           = numopts$ts,
    cs           = numopts$cs,
    noms         = numopts$noms,
    index        = d.subset$index,
    ords         = numopts$ords,
    m            = m,
    logs         = numopts$logs,
    archv        = archv,
    xmin         = d.trans$xmin,
    sqrts        = numopts$sqrts,
    lgstc        = numopts$lgstc,
    outname      = outname,
    subset.index = d.subset$index,
    autopri      = autopri,
    empri        = empri,    #change 3a 
    tolerance    = tolerance))  #change 3b
}
