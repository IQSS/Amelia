##  diag.r
##  amelia diagnostic functins
##
##  05/05/06 mb - added amelia.arg compatibility
##  07/05/06 jh - compare: changed how variable names are found, changed titles/labels, set x-axis values in matplot, colours for no imputations 
##                overimpute: added new m-name in output,
##  09/05/06 mb - overimpute: added frontend check for overimpute. 
##  15/05/06 jh - overimpute: stacking of original data, and various graphics adjustments
##  01/06/06 mb - added "gethull" and "disperse" for overdispersion diagnostic
##  19/07/06 mb - moved handling of arglists to prep.
##  01/12/06 mb - can't compare non-numerics, only use the relevant columns when
##                building compare
##  13/12/06 mb - changed for new priors.
##  26/03/07 jh - overimpute: excluded polynomials of time from missingness count, reordered ploting of ci's (smallest last), allow variable name as var argument

compare.density <- function(data=NULL,output=NULL,var=NULL,col=1:2,lwd=1,main="",frontend=F,...) {
  
  if (all(!is.data.frame(data),!is.matrix(data)))
    stop("The 'data' is not a data frame or matrix.")
  if (class(var)=="character")
    if (!(var %in% names(data)))
      stop("The variable name (var) doesn't correspond to a column in the data.")
    else
      var<-match(var,names(data))
  if (any(var>ncol(data),var<0,var%%1!=0))
    stop("The 'var' option points to a non-existant column.")
  if (!is.numeric(data[,var]))
    stop("The variable selected is not a numeric variable.")
  if (any(!is.list(output),is.null(output$amelia.arg),is.null(output$m1)))
    stop("The 'output' is not an Amelia list.")
  
  mcount<-output$amelia.arg$m
  varimp<-output[[1]][,var]
  if (identical(varimp,NA)) {
    varimp<-0
    mcount<-mcount-1
  } else {
    if (any(dim(output[[1]])!=dim(data)))
      stop("The 'output' doesn't match the data.")
    else if (!is.numeric(output[[1]][,var]))
      stop("The variable selected is not a numeric variable")
  }
  for (i in 2:output$amelia.arg$m) {
    if (identical(output[[i]],NA)) {
      mcount<-mcount-1
    } else {
      varimp<-varimp+output[[i]][,var]
      if (any(dim(output[[i]])!=dim(data)))
        stop("The 'output' doesn't match the data.")
      else if (!is.numeric(output[[i]][,var]))
        stop("The variable selected is not a numeric variable.")
    }
  }
  varimp<-varimp/mcount
  
  if (frontend)
    x11()
      
  
  vars <- data[,var]
  ratio<-length(varimp[is.na(vars)])/length(varimp[!is.na(vars)])
  varnames<-dimnames(data)[[2]]            # This will work for both data.frames AND matricies.
  main<-varnames[var]                      # This will work for both data.frames AND matricies.
  if (is.null(main))
    main<-""
  if (sum(is.na(vars)) > 0) {
    xmiss <- density(varimp[is.na(vars)],na.rm=T)
    xobs<- density(varimp[!is.na(vars)],na.rm=T)
    compplot <- matplot(x=cbind(xmiss$x,xobs$x),y=cbind(ratio*xmiss$y,xobs$y), xlab=main, ylab="relative density",type="l",lwd=lwd, lty=1,main=paste("Observed and Imputed values of",main),col=col,...)
    
    legend("topright",legend=c("Mean Imputations","Observed Values"),
                  col=col,lty=c(1,1),bg='gray90',lwd=lwd)
  } else {
    compplot <- plot(density(varimp,na.rm=T),
      xlim=c(min(varimp,na.rm=T),max(varimp,na.rm=T)), col = "blue",
      main = main,...)
    col.none=c("gray","blue")
    legend("topright",legend=c("Mean Imputations (None)","Observed Values"),
                  col=col.none,lty=c(1,1),bg='gray90')
  }
  
  invisible()
}



overimpute <- function(data,output,var,frontend=FALSE) {

  # Allow character names as arguments for "var" with data.frames

  if(is.character(var)){
    if(!is.data.frame(data)){
      stop("var must be identified by column number as dataset is not a data frame.")
    } else {
      varpos<-match(var,names(data))
      if(is.na(varpos)){
        stop("The name provided for var argument does not exist in the dataset provided.")
      } else {
      var<-varpos
      }
    }
  }
 
  prepped<-amelia.prep(data=data,m=m,idvars=idvars,priors=priors,empri=empri,
                       ts=ts,cs=cs,tolerance=tolerance,casepri=casepri,
                       polytime=polytime, lags=lags,leads=leads,logs=logs,
                       sqrts=sqrts,lgstc=lgstc, p2s=F,frontend=frontend,
                       archive=F,intercs=intercs, noms=noms,
                       startvals=startvals,ords=ords,incheck=F, collect=F,
                       outname="outdata",write.out=F,var=var,arglist=output)

  stacked.var<-match(var,prepped$subset.index[prepped$p.order])
  subset.var<-match(var,prepped$subset.index)
  if (is.na(stacked.var)) {
    if (frontend)
      tkmessageBox(message="The variable you selected doesn't exist in the Amelia output becuase it wasn't imputed.",icon="error",type="ok")
    stop("var doesn't exist in the amelia output.  It either didn't get imputed or is out of the range of columns.")
  } 
  
  means<-c()
  lowers<-c()
  uppers<-c()
  color<-c()
  AMr1<-is.na(prepped$x)
  AMr1[,stacked.var]<-TRUE
  AMp<-ncol(prepped$x)
  for (i in 1:nrow(prepped$x)) {
    if (is.na(prepped$x[i,stacked.var]))
      next()

    x<-prepped$x[i,,drop=F]
    x[1,stacked.var]<-NA
    o<-!is.na(x)
    miss<-!o
    x[is.na(x)]<-0  
    
    #o<-!AMr1[i,]
    #o[stacked.var]<-FALSE
    
    pcntmiss<-(sum(miss))/(length(miss)-sum(prepped$index==0))   # Does not include time polynomials (index==0) in the denominator
                                                                 # These are always fully observed by construction, but auxiliary.
                                                                 # Leaves constructed lags and leads, and nominal variables in count, however.
    conf<-c()
    for (k in 1:prepped$m) {
      thetareal<-output[[paste("theta",k,sep="")]]
      theta<-amsweep(thetareal,c(F,o))
      
      Ci<-matrix(0,AMp,AMp)
      hold<-chol(theta[c(FALSE,miss),c(FALSE,miss)])
      Ci[miss,miss]<-hold      
      imputations<-AMr1[i, , drop=FALSE] * ((x %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
          + (matrix(1,1,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) )
      for (j in 1:20) {             ## COULD REMOVE THIS LOOP
        junk<-matrix(rnorm(AMp), 1, AMp) %*% Ci
        xc<-x + imputations + junk
        conf<-c(conf,xc[,stacked.var])      
      }
    }
    
    scaled.conf <- (conf * prepped$scaled.sd[subset.var])  + prepped$scaled.mu[subset.var]
    varlog<-match(var,prepped$logs)      
    
    if (!is.na(varlog))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=1,xmin=prepped$xmin[varlog],sqrts=NULL,lgstc=NULL)
    if (!is.na(match(var,prepped$sqrts)))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=NULL,xmin=NULL,sqrts=1,lgstc=NULL)
    if (!is.na(match(var,prepped$lgstc)))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=NULL,xmin=NULL,sqrts=NULL,lgstc=1)
    
    #colors are based on rainbow roygbiv l->r is higher missingness  
    spectrum<-c("blue","green","orange","tomato","red")
    if (pcntmiss < .20)
      color<-c(color,spectrum[1])
    else if (pcntmiss >= .20 && pcntmiss <.40)
      color<-c(color,spectrum[2])
    else if (pcntmiss >= .40 && pcntmiss <.60)
      color<-c(color,spectrum[3])
    else if (pcntmiss >= .60 && pcntmiss <.80)
      color<-c(color,spectrum[4])
    else if (pcntmiss >=.80)
      color<-c(color,spectrum[5])
    means<-c(means,mean(scaled.conf))
    lowers<-c(lowers,sort(scaled.conf)[round(prepped$m*20*0.05)])
    uppers<-c(uppers,sort(scaled.conf)[round(prepped$m*20*0.95)])

  }

  AMr1<-is.na(prepped$x[,stacked.var])
  partial.n.order<-prepped$n.order[!AMr1]
 
  xplot<-data[partial.n.order,var] 

  addedroom<-(max(uppers)-min(lowers))*0.1

  if (frontend)
    x11()
  ci.order<-order(uppers-lowers,decreasing=TRUE)     # Allows smallest CI's to be printed last, and thus not buried in the plot.
  overplot<-plot(xplot[ci.order],means[ci.order],xlab="Observed values",ylab="Imputed values",ylim=range(c(lowers-addedroom,uppers)),type='p',main="Observed versus Imputed Values")
  segments(xplot[ci.order],lowers[ci.order],xplot[ci.order],uppers[ci.order],col=color[ci.order])

  legend("bottomright",legend=c(" 0-.2",".2-.4",".4-.6",".6-.8",".8-1"),
                  col=spectrum,lty=c(1,1),horiz=TRUE,bty="n")
  
  abline(0,1)
  return(overplot)
}
      
gethull <- function(st,tol,rots) {
  stvec<-st
  for (i in 1:length(st)) {
    addedvec<-rep(0,length(st))
    addedvec[i]<-tol*100
    newvec<-cbind(st + addedvec,st-addedvec)
    stvec<-cbind(stvec,newvec)
  }
  reduced.hull<-t(rots)%*%stvec
  return(reduced.hull)
}  

    
disperse <- function(data,m=5,p2s=TRUE,frontend=FALSE,idvars=NULL,logs=NULL,ts=NULL,cs=NULL,casepri=NULL,priors=NULL,empri=NULL,tolerance=0.00001,polytime=NULL,startvals=0,
                  lags=NULL, leads=NULL, intercs=FALSE,archive=TRUE,sqrts=NULL,lgstc=NULL,noms=NULL,incheck=T,
                  ords=NULL,dims=1,output=NULL) {


  if (frontend) {
    require(tcltk)
    tcl.window<<-tktoplevel()
    scr <- tkscrollbar(tcl.window, repeatinterval=5,
          command=function(...)tkyview(run.text,...))
    run.text<<-tktext(tcl.window,font=c("Courier",10),
          yscrollcommand=function(...)tkset(scr,...))
    tkgrid(run.text,scr)
    tkgrid.configure(scr,sticky="ns")
    tkwm.title(tcl.window,"Overdisperse Output")
    tcl("update")
  }
  
  code<-1
  prepped<-amelia.prep(data=data,m=m,idvars=idvars,priors=priors,empri=empri,ts=ts,cs=cs,
                        tolerance=tolerance,casepri=casepri,polytime=polytime,
                        lags=lags,leads=leads,logs=logs,sqrts=sqrts,lgstc=lgstc,
                        p2s=p2s,frontend=frontend,archive=archive,intercs=intercs,
                        noms=noms,startvals=startvals,ords=ords,incheck=incheck,arglist=output)
  
  if (prepped$code!=1) {
    cat("Amelia Error Code: ",prepped$code,"\n",prepped$message,"\n")
    return(list(code=prepped$code,message=prepped$message))
  }
  if (p2s) cat("-- Imputation", "1", "--")
  if (frontend) tkinsert(run.text,"end",paste("-- Imputation","1","--\n"))
  flush.console()
  thetanew<-emarch(prepped$x,p2s=p2s,thetaold=NULL,tolerance=tolerance,startvals=0,priors=prepped$priors,empri=empri,frontend=frontend,allthetas=T,collect=FALSE)
  impdata<-thetanew$thetanew
  startsmat<-matrix(0,ncol(prepped$x)+1,ncol(prepped$x)+1)
  startsmat[upper.tri(startsmat,T)]<-c(-1,impdata[,ncol(impdata)])
  startsmat<-t(startsmat)
  startsmat[upper.tri(startsmat,T)]<-c(-1,impdata[,ncol(impdata)])
  iters<-nrow(thetanew$iter.hist)+1
  
  for (i in 2:m){

    if (p2s) cat("-- Imputation", i, "--\n")
    if (frontend) tkinsert(run.text,"end",paste("-- Imputation",i,"--\n"))    
    newstarts<-rmvnorm(500,startsmat[1,2:ncol(startsmat)],startsmat[2:nrow(startsmat),2:nrow(startsmat)])
    startcov<-var(newstarts)
    startmus<-colMeans(newstarts)
    newstartsmat<-matrix(-1,ncol(prepped$x)+1,ncol(prepped$x)+1)
    newstartsmat[2:nrow(startsmat),2:nrow(startsmat)]<-startcov
    newstartsmat[1,2:nrow(startsmat)]<-startmus
    newstartsmat[2:nrow(startsmat),1]<-startmus

    thetanew<-emarch(prepped$x,p2s=p2s,thetaold=newstartsmat,tolerance=tolerance,startvals=0,priors=prepped$priors,empri=empri,frontend=frontend,allthetas=T,collect=FALSE)
    impdata<-cbind(impdata,thetanew$thetanew)
    iters<-c(iters,nrow(thetanew$iter.hist)+1)
  }
  if (dims==1)
    comps<-c(1)
  else 
    comps<-c(1,2)
  rotations<-prcomp(t(impdata))$rotation[,comps]
  reduced.imps<-t(rotations)%*%impdata
  if (frontend)
    x11()
  if (dims==1) {
    addedroom<-(max(reduced.imps)-min(reduced.imps))*0.1    
    x<-seq(iters[1])
    y<-reduced.imps[1,1:iters[1]]
    patt<-seq(1,length(x)-1)
    plot(x,y,col=1,main="Overdispersed Start Values",xlab="Number of Iterations",ylab="Largest Principle Component",xlim=c(0,max(iters)),ylim=range(c(reduced.imps-addedroom,reduced.imps)))
    segments(x[patt],y[patt],x[patt+1],y[patt+1],col=1)
    for (i in 2:length(iters)) {      
      x<-seq(iters[i])
      y<-reduced.imps[1,(sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      patt<-patt<-seq(1,length(x)-1)
      segments(x[patt],y[patt],x[patt+1],y[patt+1],col=i)
      points(x,y,col=i)    
    }
    abline(h=reduced.imps[iters[1]],lwd=2)
    legend("bottomright",legend=c("Convergence of original starting values"),lwd=2,bty="n")
   } else {
    xrange<-c((min(reduced.imps[1,])),(max(reduced.imps[1,])))
    yrange<-c((min(reduced.imps[2,])),(max(reduced.imps[2,])))
    plot(reduced.imps[1,1:iters[1]],reduced.imps[2,1:iters[1]],type="n",main="Overdispersed Starting Values",xlab="First Principle Component",ylab="Second Principle Component",col=1,xlim=xrange,ylim=yrange)
    for (i in 2:length(iters)) {
      x<-reduced.imps[1,(sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      y<-reduced.imps[2,(sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      patt<-c()
      xdiffs<-diff(x)
      ydiffs<-diff(y)
      veclength<-sqrt(xdiffs^2+ydiffs^2)
      for (j in 1: length(xdiffs))
        if (veclength[j] > xinch(1/500))
          patt<-c(patt,j)
      if (!is.null(patt))
        arrows(x[patt],y[patt],x[patt+1],y[patt+1],length=.1,col=i)
      patt<-seq(1,length(x)-1)
      segments(x[patt],y[patt],x[patt+1],y[patt+1],col=i)    
    }
    x<-reduced.imps[1,1:iters[1]]
    y<-reduced.imps[2,1:iters[1]]
    xdiffs<-diff(x)
    ydiffs<-diff(y)
    veclength<-sqrt(xdiffs^2+ydiffs^2)
    inchlength<-sqrt(sum(xyinch(1/500)^2))
    patt<-c()
    for (j in 1: length(xdiffs))
      if (veclength[j] > inchlength)
        patt<-c(patt,j)
    #if (!is.null(patt))
    #  arrows(x[patt],y[patt],x[patt+1],y[patt+1],length=.15,col=1,lwd=5)
    patt<-patt<-seq(1,length(x)-1)
    segments(x[patt],y[patt],x[patt+1],y[patt+1],col=1,lwd=1)
    dists<-gethull(st=impdata[,iters[1]],tol=tolerance,rots=rotations)
    convexhull<-chull(t(dists))
    convexhull<-c(convexhull,convexhull[1])
    lines(t(dists)[convexhull,],col="orange",pch=19,lwd=2)
    abline(h=0,lty=2)
    abline(v=0,lty=2)  
  }
  if (frontend)
    tkdestroy(tcl.window)
  return(list(impdata=impdata,p.order=prepped$p.order,iters=iters))

}    
    
