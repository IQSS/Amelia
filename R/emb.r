## Code for bootstrapped Amelia ported to R
## 17/09/05 jh - Added "subset" routine for idvars and completely missing observations
## 22/09/05 jh - Changed stack function to optionally fix column positions, changed bootx to reject some bootstraps, changed emarch to work when no data missing
## 23/09/05 mb - Added "amcheck" function to change data and check for errors, "impdata" now in format given to amelia.
## 24/09/05 jh - Modified "amcheck," added polynomials of time, added ability to impute "logicals" from data frames
## 25/09/05 jh - Finalized plumbing for observational priors
## 26/09/05 mb - Added "frontend" argument and screen box to amelia and emarch functions
## 27/09/05 jh - Added observational and empirical priors
## 28/09/05 mb - Fixed "frontend" to update the GUI after each print.
## 30/09/05 mb - "amcheck" expanded, priors passed as individual matrices
## 07/10/05 mb - Added passing of lags and multiple levels of polynomials;  expanded "amcheck" to cover these
## 08/10/05 jh - Enabled variable degree of polynomials of time, enabled interaction with cross-section
## 14/10/05 mb - Put "amcheck" into its own file
## 21/10/05 mb - Changed "stack" to "amstack" (and "unstack"); added log transformations in "amtransform"; adding "archive" option that saves a list of the settings
## 21/10/05 mb - Added a soft-crash that will print and output the error number and message.
## 24/10/05 mb - Added "sqrt" option for square root transformations, "lgstc" for logistic transformations
## 27/10/05 mb - Enabled lags and leads
## 9//11/05 mb - Enabled nominals;  added "incheck" to allow skipping amcheck;  moved dataframe->matrix conversion to framemat function.
## 15/12/05 mb - new (fixed) impute function; 
## 21/02/06 mb - added positive definite check to "startvals", now defaults to identity if not pd.
## 22/02/06 mb - penrose inverse function added in sweep; soft-crashes on a non invertible covariance matrix at the end of EM
## 23/02/06 mb - empri increases if EM hits a non-monotonic section; added 'startvals' option; added iteration history to archive;
## 21/03/06 mb - added "ords" option and added ordinal support in "unsubset"; fixed a bug in nominals that wouldn't fill in imputations;
## 22/03/06 mb - character/factors can be specified and returned correctly as ordinals;
## 08/04/06 jh - revised functions to handle large datasets, merged with parallel emb.r version
## 10/04/06 mb - added "nametonumber" function that converts column names to numbers in all of the list options
## 28/04/06 mb - extracted transformation functions to prep.r
## 29/04/06 jh - changed screen output for "p2s", ivector and icap in "indxs", revised "diff" convergence monitor to upper triangular
## 01/05/06 mb - removed "rbind" calls in emfred, impute.
## 01/06/06 mb - added "allthetas" option to emarch for overdispersion diagnostic
## 15/06/06 jh - merged with priors version changing all EM and impute procs, modified how lists are generated in indxs("icap") and amelia("impdata"). 
## 27/06/06 mb - added arglist argument to load in output from amelia or the gui. 
## 13/07/06 mb - moved gc() calls out of emfred into emarch
## 02/08/06 mb - removed data.matrix() call when calling unsubset (moved to prep), fixed impfill for char.
## 29/08/06 jh - changed tolerance defaults 
## 20/09/06 mb - new option (temp?) keep.data that will trash datasets from memory
## 01/10/06 mb - added additional info to p2s=2.
## 27/11/06 mb - new priors format
## 15/01/07 jh/mb - final version changes, degrees of freedom messages,autoprior option, modified comments, rearranged core arguments
## 10/05/07 mb - changed 'impute' to 'amelia.impute'
## 04/07/07 jh - added "emburn" option to modify convergence criteria
## 04/06/08 mb - changed the writing to GUI ('if (frontend)' calls) to remove globals
## 17/07/08 mb - fixed frontend error bug (dumping output to screen
## 22/07/08 mb - good coding update: T->TRUE/F->FALSE

## Draw from a multivariate normal distribution 
##   n: number of draws 
##   mu: vector of means 
##   vcv: variance-covariance matrix  
rmvnorm <- function(n,mu,vcv){
   return(matrix(rnorm(n*length(mu)),n,length(mu)) %*% (chol(vcv)) + (matrix(1,n,1) %*% mu ) ) 
} 

## Returns the data matrix without the rows with missing values
## (Same return as na.omit, without any attributes)
##   x: data matrix
##   can't send it a vector right now
packr<-function(x) {
	r<-is.na(x)
	sumr<-rowSums(r)
	x2<-x[sumr==0, , drop=FALSE]
	return(x2)
}

## Moore-Penrose Inverse function (aka Generalized Inverse)
##   X:    symmetric matrix
##   tol:  convergence requirement 
mpinv <- function(X, tol = sqrt(.Machine$double.eps)) {
  s <- svd(X)
	e <- s$d
	e[e > tol] <- 1/e[e > tol]
	s$v %*% diag(e,nrow=length(e)) %*% t(s$u)
}

## Create dataset bootstrapped from original dataset
## Rejects Bootstraps where an entire variable becomes missing
##   x:          data (matrix)
##   priors:     matrix of priors about means for observations
bootx<-function(x,priors=NULL){
  flag=TRUE
  AMn=nrow(x)
  while (flag){
    order<-trunc(runif(nrow(x), min=1, max=nrow(x)+1))
    xboot<-x[order,]
    if (!identical(priors,NULL)){
      priors[,1]<-match(priors[,1],order)
      priors <- priors[!is.na(priors[,1]),,drop=FALSE]
    }
    
    flag<-any(colSums(is.na(xboot))==AMn)
  }
  return(list(x=xboot,priors=priors))
}

## Put imputations into the original data format
## Converts integer values back to factors or characters
impfill<-function(x.orig,x.imp,noms,ords) {
    AMr1.orig <-is.na(x.orig)
    orig.class<-sapply(x.orig,class)          
    x.imp<-as.data.frame(x.imp[,1:ncol(x.orig)])
    for (i in 1:ncol(x.orig))
      if (class(x.orig[,i]) == "logical") 
        x.imp[,i]<-as.logical(x.imp[,i]>0.5) 

    if (!is.null(noms)) {
      AMr1.orig <-is.na(x.orig[,-noms])
      x.orig[,-noms][AMr1.orig]<-x.imp[,-noms][AMr1.orig]
      for (i in noms) {
        if (orig.class[i]=="factor")
          x.orig[is.na(x.orig[,i]),i]<- levels(x.orig[,i])[x.imp[is.na(x.orig[,i]),i]]
        else if (orig.class[i]=="character")
          x.orig[,i]<-unique(na.omit(x.orig[,i]))[x.imp[,i]]
        else
          x.orig[,i]<-x.imp[,i]    
      }                   
    } else {
       x.orig[AMr1.orig]<-x.imp[AMr1.orig]
    } 
    new.class<-sapply(x.orig,class)
    class.match<-(orig.class=="character")!=(new.class=="character")
    if (sum(class.match)!=0)
      for (i in 1:length(class.match))
        if (class.match[i])
          x.orig[,i]<-as.numeric(x.orig[,i])
      
    return(x.orig)
}

## Create Starting Values for EM Chain
startval<-function(x,startvals=0){
  
  AMp<-ncol(x)
  
  if (ncol(as.matrix(startvals)) == AMp+1 && nrow(as.matrix(startvals)) == AMp+1)       #checks for correct size of start value matrix
    if (startvals[1,1]==-1)                                       #checks for the -1 necessary for sweep
      return(startvals)

  thetast<-matrix(0,nrow=AMp+1,ncol=AMp+1)  # Create matrix of zeros
  thetast[row(thetast)==col(thetast)] <- 1  # Create Identity matrix
  thetast[1,1]<-(-1) 
  
  if (startvals==0){                            # Defaults to Identity if too few rows fully observed
    cmpr<-packr(x)
    if (nrow(cmpr)>AMp){
      means<-colMeans(cmpr)
      if (all(eigen(cov(cmpr))$values > .Machine$double.eps)) {   #Checks for positive definiteness (positive eigenvalues)
        thetast[2:(AMp+1),2:(AMp+1)]<-cov(cmpr)                   #.Machine$double.eps instead of 0 to account for rounding.
        thetast[2:(AMp+1),1]<-means
        thetast[1,2:(AMp+1)]<-means
      }
    }
  }
  return(thetast) 
}

## Create certain indicies.  Only needs to be called once, not every pattern.
## o,m,icap come from omiindxs
## ivector is i from indexm
indxs<-function(x){

  AMn<-nrow(x)
  AMr1<-is.na(x)       # True if missing. 
  AMr2<-unique(AMr1)   
  o<- !AMr2            # (or o<-AMr2==1)  Maybe == is not robust to small fluctuations
  m<- AMr2             # so put in check procedure (m<-)

## The following can be replaced by fortran .dll, although this has only moderate time savings ##
  ivector<-1  
  for(i in 2:AMn){
    ischange<- !identical(AMr1[i,],AMr1[i-1,])
    if(ischange){
      ivector<-c(ivector,i)
    }
  }
  ivector<-c(ivector,AMn+1)
#####################################################
  
##  ivector<-.Fortran("indxs",1*AMr1,as.integer(AMn),as.integer(ncol(x)),as.integer(nrow(AMr2)+1),ivector=integer(nrow(AMr2)+1))$ivector 

  icap<-vector(mode="list",nrow(AMr2))                     # This is a useful index, although no longer currently used
  for (i in 2:length(ivector)){
    icap[[i]]<-seq(ivector[i-1],ivector[i]-1)
  }

  return(list(AMr1=AMr1,AMr2=AMr2,o=o,m=m,icap=icap,ivector=ivector))
}

## Sweep function (NOTE does not take sign as sign to reverse sweep)
amsweep<-function(g,m,reverse=FALSE){
if (identical(m,vector(mode='logical',length=length(m)))) # This is check for sweeping on no elements
  {return(g)} else {                
  p<-nrow(g)
  rowsm<-sum(m)

  # Add Checks of Inputs About Here

  if (rowsm==p){
    h<-solve(g)
    h<-(-h)
  } else {   
    kseq<-1:p
    k<-kseq[m]    
    kcompl<-kseq[-k]     # we could do everything with m and !m, but only with small numbers of variables
    g11<-g[k,k]          # can not subset matricies with long logical vectors
    g12<-g[k,kcompl, drop=FALSE]   
    g21<-t(g12)
    g22<-g[kcompl,kcompl , drop=FALSE]
    
    h11a<-try(solve(g11),silent=TRUE)
    
    if (inherits(h11a,"try-error"))
      h11a<-mpinv(g11)     # This is where significant time is spent!  About as much time as in the rest of the EM step
    h11<-as.matrix((-h11a))
    if (reverse) {sgn2<- -1} else {sgn2<- 1}   
    h12<-as.matrix(sgn2 * (h11a %*% g12))
    h21<-as.matrix(t(h12))
    h22<-g22-(g21 %*% h11a %*% g12)

    hwo<-rbind(cbind(h11,h12),cbind(h21,h22))
    xordering<-c(k,kcompl)
    h<-matrix(0,p,p)
    h[xordering,xordering]<-hwo
  }
  return(h)
}
}

## EM chain architecture calls 
emarch<-function(x,p2s=TRUE,thetaold=NULL,startvals=0,tolerance=0.0001,priors=NULL,empri=NULL,frontend=FALSE,collect=FALSE,allthetas=FALSE,autopri=0.05,emburn=c(0,0)){
  if (p2s == 2) {
    cat("setting up EM chain indicies\n")
    flush.console()
  }
  
  iter.hist<-matrix(0,nrow=1,ncol=3)
  if (nrow(packr(x))<nrow(x)){          # Check for fully observed data

    if (identical(thetaold,NULL)) thetaold<-startval(x,startvals=startvals)    
    indx<-indxs(x)                      # This needs x.NA
    if (!identical(priors,NULL)){
      priors[,4]<-1/priors[,4]          # change sd to 1/sd
      priors[,3]<-priors[,3]*priors[,4] # get the precision-weighted mus
    }    

    x[is.na(x)]<-0                      # Change x.NA to x.0s       
    AM1stln<-sum(indx$m[1])==0
    count<-0
    diff<- 1+tolerance
    thetahold<-c()
    while ( (diff>0 | count<emburn[1] ) & (count<emburn[2] | emburn[2]<1) ){    # emburn[1] is a minimum EM chain length, emburn[2] is a maximum, ignored if less than 1.
      if (collect)
        gc()     
      count<-count+1
      if (p2s==1){
        if (identical((count %% 20),1)) {cat("\n")}
        if (count<10) cat(" ")
        cat(count," ",sep="")
        flush.console()    
      }
      if(p2s==2){
        if (identical((count %% 10),1)) {cat("\n")}
        if (count<10) cat(" ")
        cat(count)
        flush.console()   
      }
      if (frontend) {
        if (identical((count %% 20),1)) {
          tkinsert(getAmelia("run.text"),"end",paste("\n"))
          tksee(getAmelia("run.text"),"end")  #Makes the window scroll down as new lines appear.
        }
        if (count<10) tkinsert(getAmelia("run.text"),"end"," ")
        tkinsert(getAmelia("run.text"),"end",paste(count," ",sep=""))
        tcl("update")   #Forces tcltk to update the text widget that holds the amelia output
      }

      thetanew<-emfred(x,thetaold,indx$o,indx$m,indx$ivector,indx$icap,indx$AMr1,indx$AMr2,AM1stln=AM1stln,returntype="theta",priors=priors,empri=empri,collect=collect)
#browser()
      diff2<-sqrt(sum((thetanew-thetaold)^2))   
      diff<-(abs(thetanew-thetaold)>tolerance)
      diff<-sum(diff*upper.tri(diff,diag=TRUE))

      if (diff > iter.hist[count,1] && count > 2) {                                #checks to see if step length has increased
        mono.flag<-1

        if (autopri > 0) {
          if (sum(iter.hist[count,3],iter.hist[(count-1),3],1) == 3) {
                                                                                    #if step length has increased for more 3 steps
            if (is.null(empri)) {                                                      #the ridge prior is increased by 1%  of 
              empri<-trunc(.01*nrow(x))                                                #the rows of data, up to "autopri"% of the rows.
            } else {
              if (empri < (autopri*nrow(x))) empri<-empri+trunc(.01*nrow(x))        # This does not necessarily need to be an integer
            }
          }
        }
      } else {
        mono.flag<-0
      }
      if (all(eigen(thetanew[2:nrow(thetanew),2:ncol(thetanew)],only.values=TRUE, symmetric=TRUE)$values > .Machine$double.eps))
        sing.flag<-0
      else
        sing.flag<-1
      if(p2s==2){
        cat("(",diff,sep="")
        if (all(mono.flag == 1, count > 50))
          cat("*",sep="")
        if (sing.flag == 1)
          cat("!",sep="")
        cat(")",sep="")
        flush.console()      
      }

      iter.hist<-rbind(iter.hist,c(diff,sing.flag,mono.flag))
      if (allthetas)
        thetahold<-cbind(thetahold,(thetanew[upper.tri(thetanew,diag=TRUE)])[-1])
      thetaold<-thetanew
    }
    iter.hist<-iter.hist[2:nrow(iter.hist),]
  } else {
    if (p2s) cat("\n","No missing data in bootstrapped sample:  EM chain unnecessary")
    pp1<-ncol(x)+1                       # p (the number of variables) plus one
    means<-colMeans(x)               
    thetanew<-matrix(0,pp1,pp1)
    thetanew[1,1]<-(-1)
    thetanew[2:pp1,1]<-means
    thetanew[1,2:pp1]<-means
    thetanew[2:pp1,2:pp1]<-cov(x)              # Need to consider Priors in these cases, 
    iter.hist<-NA                              # Although not currently necessary. 
  }
  
  if (p2s) cat("\n")
  if (frontend) tkinsert(getAmelia("run.text"),"end",paste("\n"))
  if (allthetas)
    return(list(thetanew=cbind(thetahold,(thetanew[upper.tri(thetanew,diag=TRUE)])[-1]),iter.hist=iter.hist))
  return(list(thetanew=thetanew,iter.hist=iter.hist))
}

## Draw imputations for missing values from a given theta matrix
amelia.impute<-function(x,thetareal,priors=NULL,bounds=NULL,max.resample=NULL){

  indx<-indxs(x)                      # This needs x.NA 
  if (!identical(priors,NULL)){
    priors[,4]<-1/priors[,4]
    priors[,3]<-priors[,3]*priors[,4]
  }    


  
  x[is.na(x)]<-0                      # Change x.NA to x.0s       

  AM1stln<-sum(indx$m[1])==0          # Create sundry simple indicators
  o<-indx$o
  m<-indx$m
  i<-indx$ivector
  iii<-indx$icap
  AMr1<-indx$AMr1
  AMr2<-indx$AMr2
  AMp<-ncol(x)
  AMn<-nrow(x)

  I <- diag(1,AMp)                    # A reference identity matrix for speed
  
  xplay<-matrix(0,nrow=AMn,ncol=AMp)
  if (!AM1stln){
    st<-1
  } else {
    xplay[i[1]:(i[2]-1),]<-x[i[1]:(i[2]-1),]
    st<-2
  }

  if (identical(priors,NULL)){                     # No Observation Level Priors in Dataset

    for (ss in st:(length(i)-1)){
 
      theta<-amsweep(thetareal,c(FALSE,o[ss,]))

      is<-i[ss]
      isp<-i[ss+1]-1
      
      Ci<-matrix(0,AMp,AMp)
      hold<-chol(theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
      Ci[m[ss,],m[ss,]]<-hold

      
      imputations<-AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
          + (matrix(1,1+isp-is,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) )

      if (!identical(bounds,NULL)) {
        xplay[is:isp,] <- am.resample(x.ss=x[is:isp,], ci=Ci, imps=imputations,
                                      m.ss=m[ss,], bounds=bounds,
                                      max.resample=max.resample)
      } else {
        junk<-matrix(rnorm((i[ss+1]-is) * AMp), i[ss+1]-is, AMp) %*% Ci
        xplay[is:isp,]<-x[is:isp,] + imputations + junk
      }
    }

  } else {                                    # Observation Level Priors Used

    for (ss in st:(length(i)-1)){

      theta<-amsweep(thetareal,c(FALSE,o[ss,]))

      is<-i[ss]
      isp<-i[ss+1]-1

      
      imputations<- AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*%
        theta[2:(AMp+1),2:(AMp+1) , drop=FALSE]) + (matrix(1,1+isp-is,1) %*%
        theta[1,2:(AMp+1) , drop=FALSE]) )

      ## get the prior rows and non-prior rows
      priorsinpatt <- which(is:isp %in% priors[,1])
      nopri <- !(is:isp %in% priors[,1])

      ## fill in xplay for non-prior rows
      Ci<-matrix(0,AMp,AMp)
      hold<-chol(theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
      Ci[m[ss,],m[ss,]]<-hold

      junk <- matrix(0,nrow(imputations),AMp)
      
      if (!identical(bounds,NULL)) {
        xplay[nopri,] <- am.resample(x.ss=x[nopri,,drop=FALSE], ci=Ci,
                                     imps=imputations[nopri,],
                                      m.ss=m[ss,], bounds=bounds,
                                      max.resample=max.resample)
      } else {
        junk[nopri,] <- matrix(rnorm(sum(nopri)*AMp), sum(nopri), AMp) %*% Ci
        xplay[(is:isp)[nopri],]<-x[(is:isp)[nopri],,drop=FALSE] +
                      imputations[nopri,,drop=FALSE] + junk[nopri,,drop=FALSE]
      }
      
      for (jj in priorsinpatt){
        or <- (is:isp)[jj]  ## original rows
          
        priorsForThisRow <- priors[priors[,1] == or, , drop = FALSE] 
        priorsForThisRow <- priorsForThisRow[order(priorsForThisRow[,2]),,drop=FALSE]
        columnsWithPriors <- c(1:AMp) %in% priorsForThisRow[, 2]

        npr <- nrow(priorsForThisRow)
          
        # Calculate sd2
        solve.Sigma  <- am.inv(theta[c(FALSE,columnsWithPriors),
                                     c(FALSE,columnsWithPriors),drop=FALSE],
                               I[1:npr,1:npr,drop=FALSE])
       
        solve.Lambda <- matrix(0,npr,npr)
        diag(solve.Lambda) <- priorsForThisRow[,4]

        wvar <-am.inv(solve.Lambda + solve.Sigma, I[1:npr,1:npr,drop=FALSE])
          
        # Weight these together
          
        mu.miss <- (wvar) %*% (priorsForThisRow[,3] +
                   solve.Sigma  %*% imputations[jj,columnsWithPriors])
        
        imputations[jj,columnsWithPriors]<-mu.miss    
                                                    
        # update **theta**
        copy.theta <- theta
        copy.theta[c(FALSE,columnsWithPriors),c(FALSE,columnsWithPriors)]<-wvar
          
        ## Create "noise" term from updated theta
        Ci<-matrix(0,AMp,AMp)
        hold<-chol(copy.theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
        Ci[m[ss,],m[ss,]]<-hold

        # fork for the bounds resampler
        if (!identical(bounds,NULL)) {
          xplay[or,] <- am.resample(x.ss=x[or,,drop=FALSE], ci=Ci, imps=imputations[jj,],
                                      m.ss=m[ss,], bounds=bounds,
                                      max.resample=max.resample)            
                                      
        } else {
          junk[jj,]<-matrix(rnorm(AMp), 1, AMp) %*% Ci
 
          # Piece together this observation
          xplay[or,]<-x[or,] + (AMr1[or, , drop=FALSE] * (imputations[jj,] + junk[jj,]))
        }
      }
    }
  }

return(xplay)
}




## resampler - takes in data with a single miss. pattern and resamples;
##   returns an xplay for this pattern. 
am.resample <- function(x.ss, ci, imps, m.ss, bounds, max.resample) {

  # create some holders for the bounds
  AMn.ss <- nrow(x.ss)
  AMp    <- ncol(x.ss)
  ub.mat <- matrix(NA, nrow=AMn.ss, ncol=AMp)
  lb.mat <- matrix(NA, nrow=AMn.ss, ncol=AMp)
  xp.ss  <- matrix(0,  nrow=AMn.ss, ncol=AMp)

  junk <- matrix(rnorm(AMn.ss*AMp), AMn.ss, AMp) %*% ci
  # b.cols selects the columns that have bounds 
  b.cols <- c(1:AMp) %in% bounds[,1]

  # patt.bounds selects the rows of 'bounds' that are used in this pattern
  patt.bounds <- bounds[(bounds[,1] %in% c(1:AMp)[m.ss]),,drop=FALSE]

  # we only need to redraw if there are missing variables w/ bounds in
  # this pattern
  if (length(patt.bounds) > 0) {

    # put them in the right order so that we can 
    patt.bounds <- patt.bounds[order(patt.bounds[,1]),,drop=FALSE]

    # fill the holders (only in the columns that are missing and have bounds)
    lb.mat[,(b.cols & m.ss)] <- t(matrix(patt.bounds[,2],
                                           nrow=nrow(patt.bounds),ncol=AMn.ss))

    ub.mat[,(b.cols & m.ss)] <- t(matrix(patt.bounds[,3],
                                           nrow=nrow(patt.bounds),ncol=AMn.ss))
          
    # create an index of those rows left
    left <- seq(1,AMn.ss)
    samp <- 1
          
    while ((length(left) > 0) & (samp < max.resample)) {

      #if (length(left) == 1)
        #browser()      
      # these are matrices where T means it's in the bound, F means it's out
      utest <- (imps + junk) < ub.mat
      ltest <- (imps + junk) > lb.mat

      # this matrix combines the two
      btest <- utest & ltest

      # get the failing/passing cells
      fail.cells <- which(!btest, arr.ind=TRUE)
      fail.rows  <- rowSums(!btest, na.rm=TRUE) > 0
      pass.rows  <- rowSums(!btest, na.rm=TRUE) == 0

      # record the rows that we have left
      new.left <- left[fail.rows]
      
      if (sum(pass.rows) > 0) {
        xp.ss[left[pass.rows],] <- x.ss[pass.rows,,drop=FALSE] +
                                   imps[pass.rows,,drop=FALSE] +
                                   junk[pass.rows,,drop=FALSE]
      }

      left <- new.left
      
      junk <- matrix(rnorm(length(left)*AMp), length(left), AMp) %*% ci
      
      imps   <-  imps[fail.rows,,drop=FALSE]  
      ub.mat <- ub.mat[fail.rows,,drop=FALSE]
      lb.mat <- lb.mat[fail.rows,,drop=FALSE]
      x.ss   <-   x.ss[fail.rows,,drop=FALSE]
      samp <- samp+1
    }

    
    # set failing cells to their bounds
    if ((samp==max.resample) && (length(left) > 0)) {
      xp.ss[left,] <- x.ss + imps + junk
      utest <- (imps + junk) < ub.mat
      ltest <- (imps + junk) > lb.mat
      u.fails <- which(!utest, arr.ind=TRUE)
      l.fails <- which(!ltest, arr.ind=TRUE)
      xp.left <- xp.ss[left,,drop=FALSE]
      xp.left[u.fails] <- ub.mat[u.fails]
      xp.left[l.fails] <- lb.mat[l.fails]
      xp.ss[left,] <- xp.left
    }
          
  } else {
    xp.ss <- x.ss + imps + junk 
  }
  return(xp.ss)
}
      




## Single EM step (returns updated theta)
## the "x" passed to emfred is x.0s (missing values replaced with zeros)
emfred<-function(x,thetareal,o,m,i,iii,AMr1,AMr2,AM1stln,returntype="theta",priors=NULL,empri=NULL,collect=FALSE){
AMp<-ncol(x)
AMn<-nrow(x)

I <- diag(1,AMp)

hmcv<-matrix(0,nrow=AMp,ncol=AMp)
xplay<-matrix(0,nrow=AMn,ncol=AMp)
if (!AM1stln){
  st<-1
} else {
  xplay[i[1]:(i[2]-1), ]<-x[i[1]:(i[2]-1),]
  st<-2
}

if (identical(priors,NULL)){                     # No Observation Level Priors in Dataset
  for (ss in st:(length(i)-1)){

    theta<-amsweep(thetareal,c(FALSE,o[ss,]))

    is<-i[ss]
    isp<-i[ss+1]-1
    hmcv[m[ss,],m[ss,]]<-hmcv[m[ss,],m[ss,]] + ((1+isp-is) * theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
    
    imputations<-AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
        + (matrix(1,1+isp-is,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) ) 
    xplay[is:isp,]<-x[is:isp,] + imputations 

  }
} else {                                    # Observation Level Priors Used
  
  for (ss in st:(length(i)-1)){

    theta<-amsweep(thetareal,c(FALSE,o[ss,]))
    
    is<-i[ss]
    isp<-i[ss+1]-1

    
    
    
    imputations <- AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
        + (matrix(1,1+isp-is,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) ) 

    # find the priors for this pattern
    priorsinpatt <- which(is:isp %in% priors[,1])

    
    # update hmcv for non-prior obs
    hmcv[m[ss,],m[ss,]] <- hmcv[m[ss,],m[ss,]] +
                          (1+isp-is-length(priorsinpatt))*theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])]

    
    # for rows with priors: change the relevant imputation cells
    # and update the hmcv properly
    for (jj in priorsinpatt) {
      # Prior specified for this observation

      or <- (is:isp)[jj]  ## original row 
        
      ## maybe we should sort priors earlier? do we need to?
      priorsForThisRow <- priors[priors[,1] == or, , drop = FALSE] 
      priorsForThisRow <- priorsForThisRow[order(priorsForThisRow[,2]),,drop=FALSE]
      columnsWithPriors <- c(1:AMp) %in% priorsForThisRow[, 2]

      npr <- nrow(priorsForThisRow)

      # Calculate sd2
      solve.Sigma  <- am.inv(theta[c(FALSE,columnsWithPriors),
                                   c(FALSE,columnsWithPriors),drop=FALSE] ,
                             I[1:npr,1:npr,drop=FALSE])
       
      solve.Lambda <- matrix(0, nrow(priorsForThisRow),nrow(priorsForThisRow))
      diag(solve.Lambda) <- priorsForThisRow[,4]
      
      # Update "hmcv" 
      copy.theta<-theta

      # Make a copy of theta
      copy.theta[ c(FALSE,columnsWithPriors) ,c(FALSE,columnsWithPriors) ] <- am.inv(solve.Lambda + solve.Sigma,I[1:nrow(priorsForThisRow),1:nrow(priorsForThisRow),drop=FALSE])                    

      
      # Weight these together
      
      mu.miss <- (copy.theta[c(FALSE,columnsWithPriors),c(FALSE,columnsWithPriors)]) %*%
                 (priorsForThisRow[,3] +
                  solve.Sigma  %*% imputations[jj,columnsWithPriors])
        
      imputations[jj,columnsWithPriors]<-mu.miss 

      # Overwrite prior locations
      hmcv[m[ss,],m[ss,]] <- hmcv[m[ss,],m[ss,]] + copy.theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])]


      
    }

    ## now that we've updated the imputations, add them to xplay
    xplay[is:isp,]<-x[is:isp,] + imputations 
    if (collect)
      gc()
  }
}

if (returntype=="theta"){

  interior<-(t(xplay) %*% xplay) + hmcv
  music<-  as.matrix(colSums(xplay))

  if (!identical(empri,NULL)){  
    if (empri>0){  
      hold<-matrix(0,AMp,AMp)
      hold[row(hold)==col(hold)]<- empri
      simple<-((1/AMn) * (music %*% t(music)))
      interior<-( (AMn/(AMn + empri + AMp +2)) * (interior - simple + hold) ) + simple
    }
  } 
    
  thetanew<-rbind( cbind(as.matrix(AMn),t(music)), cbind(music,interior) )

  thetanew<-thetanew/AMn
  sweeppos<-(1:(AMp+1)==1)
  thetanew<-amsweep(thetanew,sweeppos)
  
  return(thetanew)

  } else {
  return(xplay)
  }
}


am.inv <- function(a,b,tol=.Machine$double.eps) {
  if (length(a)==1)
    return(1/a)
  storage.mode(a) <- "double"
  size <- ncol(a)
  .Call("La_chol2inv",.Call("La_chol",a,PACKAGE="base"),size,PACKAGE="base")
  
  #storage.mode(a) <- "double"
  #storage.mode(b) <- "double"
  #.Call("La_dgesv",a,b,tol)
}

## Core amelia function
amelia<-function(data,m=5,p2s=1,frontend=FALSE,idvars=NULL,
                 ts=NULL,cs=NULL,polytime=NULL,intercs=FALSE,
                 lags=NULL,leads=NULL,startvals=0,tolerance=0.0001,
                 logs=NULL,sqrts=NULL,lgstc=NULL,noms=NULL,ords=NULL,
                 incheck=TRUE,collect=FALSE,outname="outdata",
                 write.out=TRUE,archive=TRUE,arglist=NULL,keep.data=TRUE, 
                 empri=NULL,priors=NULL,autopri=0.05,
                 emburn=c(0,0),bounds=NULL,max.resample=100) {

  #Generates the Amelia Output window for the frontend
  if (frontend) {
    require(tcltk)
    putAmelia("gui.container",tktoplevel())
    scr <- tkscrollbar(getAmelia("gui.container"), repeatinterval=5,
          command=function(...)tkyview(getAmelia("run.text"),...))
    putAmelia("run.text", tktext(getAmelia("gui.container"),font=c("Courier",10),
          yscrollcommand=function(...)tkset(scr,...)))
    tkgrid(getAmelia("run.text"),scr)
    tkgrid.configure(scr,sticky="ns")
    tkwm.title(getAmelia("gui.container"),"Amelia Output")
    tcl("update")
  }
  if (p2s==2) {
    cat("\namelia starting\n")
    flush.console()
  }
             
  code<-1   
  
  prepped<-amelia.prep(data=data,m=m,idvars=idvars,empri=empri,ts=ts,cs=cs,
                       tolerance=tolerance,polytime=polytime,
                       lags=lags,leads=leads,logs=logs,sqrts=sqrts,lgstc=lgstc,
                       p2s=p2s,frontend=frontend,archive=archive,intercs=intercs,
                       noms=noms,startvals=startvals,ords=ords,incheck=incheck,
                       collect=collect,outname=outname,write.out=write.out,
                       arglist=arglist,priors=priors,autopri=autopri,bounds=bounds,
                       max.resample=max.resample)
  
  if (prepped$code!=1) {
    cat("Amelia Error Code: ",prepped$code,"\n",prepped$message,"\n")
    return(list(code=prepped$code,message=prepped$message))
  }
  impdata<-vector(mode="list",(2*m))
  for (i in 1:m){
    
    if (p2s==2) {
      cat("running bootstrap\n")
      flush.console()
    }
    
    x.boot<-bootx(prepped$x,prepped$priors) 
    x.stacked<-amstack(x.boot$x,colorder=FALSE,x.boot$priors)   # Don't reorder columns thetanew will not align with d.stacked$x

    if (p2s) cat("-- Imputation", i, "--\n")
    if (frontend) tkinsert(getAmelia("run.text"),"end",paste("-- Imputation",i,"--\n"))
    flush.console()

    thetanew<-emarch(x.stacked$x,p2s=p2s,thetaold=NULL,tolerance=tolerance,startvals=startvals,x.stacked$priors,empri=empri,frontend=frontend,collect=collect,autopri=prepped$autopri,emburn=emburn)
    if (archive){
      prepped$archv[[paste("iter.hist",i,sep="")]]<-thetanew$iter.hist
    }

    if (keep.data) {
      impdata[[m+i]]<-thetanew$thetanew
      names(impdata)[m+i]<-paste("theta",i,sep="")
    } else {
      impdata[[m+i]]<-NA
    }

    if
    (any(eigen(thetanew$thetanew[2:nrow(thetanew$thetanew),2:ncol(thetanew$thetanew)], only.values=TRUE, symmetric=TRUE)$values < .Machine$double.eps)) {
      impdata[[i]]<-NA
      code<-2
      cat("\n\nThe resulting variance matrix was not invertible.  Please check
your data for highly collinear variables.\n\n")
      if (frontend) {
        tkinsert(getAmelia("run.text"),"end","\n\nThe resulting variance matrix was not invertible.  Please check your data for highly collinear variables.\n\n")
      }
      next()
        
    }
    
    ximp<-amelia.impute(prepped$x, thetanew$thetanew,priors=prepped$priors,bounds=prepped$bounds,max.resample)
    ximp<-amunstack(ximp,n.order=prepped$n.order,p.order=prepped$p.order)     
    ximp<-unscale(ximp,mu=prepped$scaled.mu,sd=prepped$scaled.sd)    
    ximp<-unsubset(x.orig=prepped$trans.x,x.imp=ximp,blanks=prepped$blanks,idvars=prepped$idvars,ts=prepped$ts,cs=prepped$cs,polytime=polytime,intercs=intercs,noms=prepped$noms,index=prepped$index,ords=prepped$ords)
    ximp<-untransform(ximp,logs=prepped$logs,xmin=prepped$xmin,sqrts=prepped$sqrts,lgstc=prepped$lgstc)

    if (p2s==2) {
      cat("\n saving and cleaning\n")
      flush.console()
    }

    ## here we deal with the imputed matrix.

    # first, we put the data into the output list and name it
    impdata[[i]]<-impfill(x.orig=data,x.imp=ximp,noms=prepped$noms,ords=prepped$ords)
    names(impdata)[i]<-paste("m",i,sep="")

    # if the user wants to save it, do that
    if (write.out){
      write.csv(impdata[[i]], file=paste(prepped$outname,i,".csv",sep=""))
    }

    # if the user wants to save memory, dump the copy in memory. 
    if (!keep.data) {      
      impdata[[i]]<-NA
    }

    
    if (p2s) cat("\n")
    if (frontend) tkinsert(getAmelia("run.text"),"end","\n")



  }
  

  impdata$code<-code
  if (code == 2)
    impdata$message<-paste("One or more of the imputations resulted in a
covariance matrix that was not invertible.")
  if (frontend) tkinsert(getAmelia("run.text"),"end",paste(impdata$message,"\n"))
  if (archive)
    impdata$amelia.args<-prepped$archv

  return(impdata)  
}                            
