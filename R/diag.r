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
##  28/03/07 jh - disperse: changed tolerance and empri handling.
##  03/04/07 jh - disperse: changed 1d plot settings, number of colors, minor edits to "patt" construction.
##  10/04/07 jh - created sigalert function to view disperse principal components.
##  22/07/08 mb - good coding update: T->TRUE/F->FALSE
##  10/02/09 mb - compare: added lwd, col, main, lab, etc for user
##                control, added scale so that users can control scaling,
##                uses amelia class
##                overimpute: uses amelia class, added lwd, col, main, lab, etc for user
##                disperse: now uses amelia class


compare.density <- function(output,var,col=c("red","black"),scaled=FALSE,lwd=1,main,xlab,ylab,legend=TRUE,frontend=FALSE,...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")

  data <- getOriginalData(output)

  ## Checks on if the variable makes sense to plot.
  if (class(var)=="character")
    if (!(var %in% names(data)))
      stop("The variable name (var) doesn't correspond to a column in the data.")
    else
      var<-match(var,names(data))
  if (any(var>ncol(data),var<0,var%%1!=0))
    stop("The 'var' option points to a non-existant column.")
  if (var %in% output$arguments$idvar)
    stop("the variable selected was marked as an idvar")
  
  ## We need to clean the data to make sure that
  ## we're not going to run into NAs
  mcount <- sum(!is.na(output$imputations))
  imputed <- (1:output$m)[!is.na(output$imputations)]

  ## create an empty vector to sum across
  varimp <- matrix(NA, nrow(data), mcount)
  
  for (i in 1:mcount) {
    varimp[,i] <- output$imputations[[imputed[i]]][,var]
  }
  if (var %in% c(output$arguments$noms, output$arguments$ords)) {
    leg.text <- "Modal Imputations"
    varimp <- apply(varimp, 1, function(x) as.numeric(names(which.max(table(x)))))
  } else {
    leg.text <- "Mean Imputations"
    varimp <- rowMeans(varimp)
  }
  
  if (frontend)
    x11()
      
  
  vars <- data[,var]
  if (scaled) 
    ratio <- sum(is.na(vars))/sum(!is.na(vars))
  else
    ratio <- 1
  varnames<-dimnames(data)[[2]]            # This will work for both data.frames AND matricies.
  vname<-varnames[var]                     # This will work for both data.frames AND matricies.

  
  if (sum(is.na(vars)) > 0) {
    oiDetect <- (sum(output$missMatrix[,var]) + sum(!is.na(vars))) > length(vars)
    if (missing(main)) {
      if (oiDetect) {
        main <-  paste("Observed and Overimputed values of",vname)
      } else { 
        main <- paste("Observed and Imputed values of",vname)
      }
    }
    if (missing(xlab)) {
      xlab <- paste(vname,"  --  Fraction Missing:",round(mean(is.na(vars)),digits=3))

    }
    if (missing(ylab)) {
      ylab <- "Relative Density"
    }
    
    xmiss <- density(varimp[output$missMatrix[,var]],na.rm=TRUE)
    xobs  <- density(vars[!is.na(vars)],na.rm=TRUE)
    compplot <- matplot(x=cbind(xmiss$x,xobs$x),y=cbind(ratio*xmiss$y,xobs$y), xlab=xlab, ylab=ylab,type="l",lwd=lwd, lty=1,main=main,col=col,...)
    if (legend) {
      legend("topright",legend=c(leg.text,"Observed Values"),
             col=col,lty=c(1,1),bg='gray90',lwd=lwd)
    }
  } else {
    if (missing(main)) {
      main <- paste("Observed values of",vname)
    }
    if (missing(xlab)) {
      xlab <- vname
    }
    if (missing(ylab)) {
      ylab <- "Relative Density"
    }
    
    compplot <- plot(density(varimp,na.rm=TRUE), col = "blue",
      main = main,...)
    col.none=c("gray","blue")

    if (legend) {
      legend("topright",legend=c("Mean Imputations (None)","Observed Values"),
             col=col.none,lty=c(1,1),bg='gray90')
    }
  }
  
  invisible()
}


##
## overimpute - imputes observed values to check the imputation model
##
##  INPUTS:output         - output from amelia run (class "amelia")
##         var            - column number or variable name to overimpute
##         legend         - should we add a lengend to the plot? (TRUE/FALSE)
##         xlab,ylab,main - graphical parameters
##         frontend       - logical for printing to tcl/tk window
##
## mb 02/02/09 - changed calls to "amelia" class of output
##             - added ability to pass xlab,main,legend, etc
##


overimpute <- function(output,var,legend=TRUE,xlab,ylab,main,frontend=FALSE,...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")

  
  data <- getOriginalData(output)
  origAMr1 <- is.na(data)

  # Allow character names as arguments for "var" with data.frames

  if(is.character(var)){
    if(!is.data.frame(data)){
      stop("var must be identified by column number as dataset is not a data frame.")
    } else {
      varpos<-match(var,colnames(data))
      if(is.na(varpos)){
        stop("The name provided for var argument does not exist in the dataset provided.")
      } else {
      var<-varpos
      }
    }
  }


  ## The argument list for an amelia output is now
  ## at "output$arguments"
  prepped<-amelia.prep(x=data,arglist=output$arguments, incheck=FALSE)

  stacked.var<-match(var,prepped$subset.index[prepped$p.order])
  subset.var<-match(var,prepped$subset.index)
  if (!is.null(prepped$blanks)) 
    fully.missing <- origAMr1[-prepped$blanks, var][prepped$n.order]
  else
    fully.missing <- origAMr1[, var][prepped$n.order]
  
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
  ## if (sum(!AMr1[,stacked.var]) == 0){
  ##   if (frontend) {
  ##     tkmessageBox(parent = getAmelia("gui"),
  ##                  message="The variable needs to have at least one fully observed cell.",icon="error",type="ok")
  ##   }
  ##   stop("function needs at least one fully observed cell in 'var'.")
  ## }
  AMr1[,stacked.var]<-TRUE
  AMp<-ncol(prepped$x)
  for (i in 1:nrow(prepped$x)) {
    if (fully.missing[i]) {
        next()
    }

    x<-prepped$x[i,,drop=FALSE]
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
    for (k in 1:output$m) {

      ## The theta matrix is now stored in a array with
      ## dimensions c(vars+1,vars+1,m), so this grabs
      ## the kth theta matrix.
      
      thetareal<-output$theta[,,k]
      theta <- amsweep(thetareal, c(FALSE,o))
      Ci<-matrix(0,AMp,AMp)
      hold<-chol(theta[c(FALSE,miss),c(FALSE,miss)])
      Ci[miss,miss]<-hold      
      imputations<-AMr1[i, , drop=FALSE] * ((x %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
          + (matrix(1,1,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) )
     ## for (j in 1:20) {             ## COULD REMOVE THIS LOOP 
     ##                                 ##(mb: I removed it for speed)
     ##   junk<-matrix(rnorm(AMp), 1, AMp) %*% Ci
     ##   xc<-x + imputations + junk
     ##   conf<-c(conf,xc[,stacked.var])      
     ## }
      junk <- matrix(rnorm(20*AMp), 20, AMp) %*% Ci
      xc <- matrix(x,20,AMp,byrow=TRUE) +
            matrix(imputations, 20, AMp, byrow=TRUE) +
            junk
      conf <- c(conf, xc[,stacked.var])
    }
    
    scaled.conf <- (conf * prepped$scaled.sd[subset.var])  + prepped$scaled.mu[subset.var]
    varlog<-match(var,prepped$logs)      
    
    if (!is.na(varlog))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=1,xmin=prepped$xmin[varlog],sqrts=NULL,lgstc=NULL)
    if (!is.na(match(var,prepped$sqrts)))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=NULL,xmin=NULL,sqrts=1,lgstc=NULL)
    if (!is.na(match(var,prepped$lgstc)))
      scaled.conf <- untransform(as.matrix(scaled.conf),logs=NULL,xmin=NULL,sqrts=NULL,lgstc=1)
    
    #colors are based on rainbow roygbiv l->r is higher missingness  \
    blue <- rgb(0,0,1, alpha = 0.75)
    green <- rgb(0,.75,0, alpha = 0.75)
    orange <- rgb(1, 0.65,0, alpha = 0.75)
    tomato <- rgb(1, 0.39, 0.28, alpha = 0.75)
    red <- rgb(0.75, 0, 0, alpha = 0.75)
    spectrum<-c(blue, green, orange, tomato, red)

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
    lowers<-c(lowers,sort(scaled.conf)[round(output$m*20*0.05)])
    uppers<-c(uppers,sort(scaled.conf)[round(output$m*20*0.95)])

  }

  #AMr1<-is.na(prepped$x[,stacked.var])
  #partial.n.order<-prepped$n.order[!origAMr1]

  if (is.null(prepped$blanks)) {
    xplot<-data[prepped$n.order,var][!fully.missing]   
  } else {
    xplot<-data[-prepped$blanks,][prepped$n.order,var][!fully.missing]   
  }
  

  addedroom<-(max(uppers)-min(lowers))*0.1

  if (missing(xlab)) {
    xlab <- "Observed Values"
  }
  if (missing(ylab)) {
    ylab <- "Imputed Values"
  }
  if (missing(main)) {
    main <- paste("Observed versus Imputed Values of",colnames(data)[var])
  }

  if (frontend) {
    if (.Platform$OS.type == "windows") {
      windows()
    } else {
      x11()      
    }
  }
  ci.order<-order(uppers-lowers,decreasing=TRUE)     # Allows smallest CI's to be printed last, and thus not buried in the plot.
  overplot<-plot(xplot[ci.order],means[ci.order],xlab=xlab,ylab=ylab,ylim=range(c(lowers-addedroom,uppers)),type='p',main=main,
                 col=color[ci.order], pch = 19,...)
  segments(xplot[ci.order],lowers[ci.order],xplot[ci.order],uppers[ci.order],col=color[ci.order])
  if (legend) {
    legend("bottomright",legend=c(" 0-.2",".2-.4",".4-.6",".6-.8",".8-1"),
           col=spectrum,lty=c(1,1),horiz=TRUE,bty="n")
  }
  
  abline(0,1)
  invisible(overplot)
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

    
disperse <- function(output, m = 5, dims = 1, p2s = 0, frontend=FALSE,...) {

  if (!("amelia" %in% class(output)))
    stop("The 'output' is not Amelia output.")

  ## The original data is the imputed data with the
  ## imputations marked to NA. These two lines do that
  data <- getOriginalData(output)
  
  if (frontend) {
    require(tcltk)
    putAmelia("output.log", c(getAmelia("output.log"), "==== Overdispersion Output ====\n"))
  }

  # prep the data and arguments
  prepped<-amelia.prep(x=data, arglist=output$arguments)

  if (p2s) cat("-- Imputation", "1", "--")
  if (frontend) {
    putAmelia("output.log", c(getAmelia("output.log"), paste("-- Imputation","1","--\n")))
  }
  flush.console()

  # run EM, but return it with the theta at each iteration
  thetanew<-emarch(prepped$x,p2s=p2s,thetaold=NULL,tolerance=prepped$tolerance,startvals=0,priors=prepped$priors,empri=prepped$empri,frontend=frontend,allthetas=TRUE,collect=FALSE)  #change 4

  # thetanew is a matrix whose columns are vectorized upper triangles of theta
  # matrices for each iteration. thus, there are k(k+1)/2 rows.
  impdata<-thetanew$thetanew

  # we'll put the theta of the last iteration into a new starting theta
  startsmat<-matrix(0,ncol(prepped$x)+1,ncol(prepped$x)+1)
  startsmat[upper.tri(startsmat,TRUE)]<-c(-1,impdata[,ncol(impdata)])     
  startsmat<-t(startsmat)
  startsmat[upper.tri(startsmat,TRUE)]<-c(-1,impdata[,ncol(impdata)])
  iters<-nrow(thetanew$iter.hist)+1

  for (i in 2:m){

    if (p2s) cat("-- Imputation", i, "--\n")
    if (frontend) {
      putAmelia("output.log", c(getAmelia("output.log"), paste("-- Imputation",i,"--\n")))
    }    

    # get a noisy sample of data from the that starting value (which is the
    # Amelia answer) and use that to estimate a new starting theta (mus/vcov)
    newstarts<-rmvnorm(round(2.5*ncol(prepped$x)),startsmat[1,2:ncol(startsmat)],startsmat[2:nrow(startsmat),2:nrow(startsmat)])
    startcov<-var(newstarts)
    startmus<-colMeans(newstarts)
    newstartsmat<-matrix(-1,ncol(prepped$x)+1,ncol(prepped$x)+1)
    newstartsmat[2:nrow(startsmat),2:nrow(startsmat)]<-startcov
    newstartsmat[1,2:nrow(startsmat)]<-startmus
    newstartsmat[2:nrow(startsmat),1]<-startmus

    # grab the iteration history of the thetas
    thetanew<-emarch(prepped$x,p2s=p2s,thetaold=newstartsmat,tolerance=prepped$tolerance,startvals=0,priors=prepped$priors,empri=prepped$empri,frontend=frontend,allthetas=TRUE,collect=FALSE)  # change 5
    impdata<-cbind(impdata,thetanew$thetanew)
    iters<-c(iters,nrow(thetanew$iter.hist)+1)
  }
  if (dims==1)
    comps<-c(1)
  else 
    comps<-c(1,2)

  # reduce the dimenionality from k(k+1)/2 to 1 or 2 via principle components
  rotations<-prcomp(t(impdata))$rotation[,comps]
  reduced.imps<-t(rotations)%*%impdata

  cols <- rainbow(m)
  # plot the imputations
  if (frontend)
    x11()
  if (dims==1) {
    addedroom<-(max(reduced.imps)-min(reduced.imps))*0.1    
    x<-seq(iters[1])
    y<-reduced.imps[1,1:iters[1]]
    patt<-seq(1,length(x)-1)
    plot(x,y,col=1,main="Overdispersed Start Values",xlab="Number of Iterations",ylab="Largest Principle Component",xlim=c(0,max(iters)),ylim=range(c(reduced.imps-addedroom,reduced.imps)),type="n")
    segments(x[patt],y[patt],x[patt+1],y[patt+1],col=cols[1])
    for (i in 2:length(iters)) {      
      x<-seq(iters[i])
      y<-reduced.imps[1,(sum(iters[1:(i-1)])+1):(sum(iters[1:i]))]
      patt<-seq(1,length(x)-1)                          
      segments(x[patt],y[patt],x[patt+1],y[patt+1],col=cols[i])
      #points(x,y,col=i)    
    }
    abline(h=reduced.imps[iters[1]],lwd=2)
    legend("bottomright",legend=c("Convergence of original starting values"),lwd=2,bty="n")
   } else {
    xrange<-c((min(reduced.imps[1,])),(max(reduced.imps[1,])))
    yrange<-c((min(reduced.imps[2,])),(max(reduced.imps[2,])))
    plot(reduced.imps[1,1:iters[1]],reduced.imps[2,1:iters[1]],type="n",main="Overdispersed Starting Values",xlab="First Principle Component",ylab="Second Principle Component",col=cols[1],xlim=xrange,ylim=yrange)
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
        arrows(x[patt],y[patt],x[patt+1],y[patt+1],length=.1,col=cols[i])
      patt<-seq(1,length(x)-1)
      segments(x[patt],y[patt],x[patt+1],y[patt+1],col=cols[i])    
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
    patt<-seq(1,length(x)-1)                                   
    segments(x[patt],y[patt],x[patt+1],y[patt+1],col=cols[1],lwd=1)
    dists<-gethull(st=impdata[,iters[1]],tol=prepped$tolerance,rots=rotations)
    convexhull<-chull(t(dists))
    convexhull<-c(convexhull,convexhull[1])
    lines(t(dists)[convexhull,],col="orange",pch=19,lwd=2)
    abline(h=0,lty=2)
    abline(v=0,lty=2)  
  }
  #if (frontend)
  #  tkdestroy(getAmelia("tcl.window"))
  
  invisible(list(impdata=impdata,p.order=prepped$p.order,index=prepped$index,iters=iters,rotations=rotations,dims=dims))

}    
   

sigalert<-function(data,disperse.list,output,notorious=5){

  k<-length(disperse.list$p.order)+1

  # Construct Variable Names for all variables constructed in Imputation Model. 
  # This uses the "index" which details all the variables included in the imputation model.
  # The index is in the unstacked variable order.
  # Possibly, if this is useful elsewhere, this might be moved to "prep.r".

  varnm<-NULL
  lag.count<-0
  lead.count<-0
  poly.count<-0
  unknown.count<-0
  
  for(i in 1:(k-1)){
    if(identical(disperse.list$index[i],-0.5)){
      lag.count<-lag.count+1
      varnm<-c(varnm,paste("lag",lag.count))      
    }else if(identical(disperse.list$index[i],0.5)){
      lead.count<-lead.count+1
      varnm<-c(varnm,paste("lead",lead.count))      
    }else if(identical(disperse.list$index[i],0)){
      poly.count<-poly.count+1
      varnm<-c(varnm,paste("polytime",poly.count))      
    }else if(disperse.list$index[i]>=1){
      varnm<-c(varnm,names(data[disperse.list$index[i]]))      # Check what this does with matricies?
    }else{
      unknown.count<-unknown.count+1
      varnm<-c(varnm,paste("unknown",unknown.count))      
    }
  }

#  WARNING: Currently assumes rotations is a vector.  If dim=2, rotations is a matrix.
#  if(!identical(disperse.list$dims,1)){
#    disperse.list$rotations<-disperse.list$rotations[1,]
#  }

  # This is a flag vector that identifies the largest values in the first principal component.

  largest.rotations<-disperse.list$rotations * 0
  largest.rotations[order(abs(disperse.list$rotations),decreasing=TRUE)[1:notorious]]<-1
    
  # This is a matrix of the size of theta, which has a 1 in the positions of the largest 
  # contributions to the first principal component. 
  # (largest corresponding elements of disperse.list$rotations)

  map<-matrix(0,k,k)
  map[upper.tri(map,TRUE)]<-c(0,largest.rotations)     
  map<-t(map)
  map[upper.tri(map,TRUE)]<-c(0,largest.rotations) 
  map[c(1,disperse.list$p.order+1),c(1,disperse.list$p.order+1)]<-map                         # Rearrange to unstacked variable positions

  print(abs(map))

  gtz<-function(a)
    return(sum(a)>0)
  row.keep<-apply(map,1,gtz)
  col.keep<-apply(map,2,gtz)

  # This is the submatrix of rotations, reshaped as a theta matrix, with the largest elements.

  prcomp.matrix<-matrix(0,k,k)
  prcomp.matrix[upper.tri(prcomp.matrix,TRUE)]<-c(0,disperse.list$rotations)     
  prcomp.matrix<-t(prcomp.matrix)
  prcomp.matrix[upper.tri(prcomp.matrix,TRUE)]<-c(0,disperse.list$rotations) 
  prcomp.matrix[c(1,disperse.list$p.order+1),c(1,disperse.list$p.order+1)]<-prcomp.matrix     # Rearrange to unstacked variable positions

  # This is the submatrix that we want to represent
  portal<-prcomp.matrix[row.keep,col.keep]
  portalsize<-ncol(portal)

  portal.row.names<-varnm[row.keep]               # In symmetric matricies, these are the same.
  portal.col.names<-varnm[col.keep]               # In symmetric matricies, these are the same.

  # This is a matrix that gives the relative rank of every element.
  col.map<-matrix(0,portalsize,portalsize)
  col.portal<-rank(abs(portal[upper.tri(portal,TRUE)]))
  col.map[upper.tri(col.map,TRUE)]<-col.portal     
  col.map<-t(col.map)
  col.map[upper.tri(col.map,TRUE)]<-col.portal 

  # This creates a continuous color palette of the correct size.
  n.unique<-sum(upper.tri(matrix(1,portalsize,portalsize),TRUE))
  Lab.palette<-colorRampPalette(c("white","yellow","red"), space = "Lab")
  my.palette<-Lab.palette(n.unique)

  # Plot the submatrix to be represented.

  plot.new()
  plot.window(xlim=c(-2,portalsize+1), ylim=c(1,portalsize+3))
  for(i in 1:portalsize){
    text(x=1,y=portalsize-i+1+0.5,pos=2,labels=portal.row.names[i])        # Row variable names
    for(j in 1:portalsize){
      rect(xleft=j, ybottom=portalsize-i+1, xright=j+1, ytop=portalsize-i+2, density = NULL, angle = 45,
        col = my.palette[col.map[i,j]], border = NULL, lty = par("lty"), lwd = par("lwd"))
      text(x=j+0.5,y=portalsize-i+1+0.5,labels=as.character(round(portal[i,j]*100)/100) )      # SHOULD FIND BETTER SIG FIGS HACK
    }
  }
  for(j in 1:portalsize){
    text(x=j+0.6,y=portalsize+1.1,pos=2,labels=portal.col.names[j],srt=270)  # Column variable names.
  }

  return(NULL)
}


tscsPlot <- function(output, var, cs, draws = 100, conf = .90,
                      misscol = "red", obscol = "black", xlab, ylab, main,
                      pch, ylim, xlim, frontend = FALSE, ...) {
  if (missing(var))
    stop("I don't know which variable (var) to plot")
  if (missing(cs))
    stop("case name (cs) is not specified")
  if (is.null(output$arguments$ts) || is.null(output$arguments$cs))
    stop("both 'ts' and 'cs' need to be set in the amelia output")
  if (!("amelia" %in% class(output)))
    stop("the 'output' is not Amelia output")

  data <- getOriginalData(output)

  # Allow character names as arguments for "var" with data.frames

  if(is.character(var)){
    if(!is.data.frame(data)){
      stop("'var' must be identified by column number as dataset is not a data frame")
    } else {
      varpos<-match(var,colnames(data))
      if(is.na(varpos)){
        stop("the name provided for 'var' argument does not exist in the dataset provided")
      } else {
      var<-varpos
      }
    }
  }

  units <- unique(data[,output$arguments$cs])
  if (!(cs %in% units))
    stop("the cross-section unit is not in the data")

  unit.rows <- which(data[,output$arguments$cs]==cs)
  time <- data[unit.rows, output$arguments$ts]
  miss <- output$missMatrix[unit.rows, var] == 1
  

  prepped <- amelia.prep(x = data, arglist=output$arguments)
  cross.sec <- prepped$x[!is.na(match(prepped$n.order, unit.rows)),]
  stacked.var<-match(var,prepped$subset.index[prepped$p.order])
  subset.var<-match(var,prepped$subset.index)
  imps <- array(NA, dim=c(nrow(cross.sec), draws))

  drawsperimp <- (1/output$m)*draws
  if (sum(miss) > 0) { 
    for (i in 1:draws) {
      currtheta <- output$theta[,,ceiling(i/drawsperimp)]
      imps[,i] <- amelia.impute(x = cross.sec, thetareal = currtheta,
                                bounds = prepped$bounds,
                                priors = prepped$priors,
                                max.resample = output$arguments$max.resample)[,stacked.var]
    }
    
    imps <- imps*prepped$scaled.sd[subset.var] + prepped$scaled.mu[subset.var]
    if (var %in% output$arguments$logs) {
      imps <- exp(imps)+prepped$xmin[which(var==output$arguments$logs)]
    }
    if (var %in% output$arguments$sqrt) {
      imps <- imps^2
    }
    if (var %in% output$arguments$lgstc) {
      imps <- exp(imps)/(1+exp(imps))
    }
  
    outoforder <- match(prepped$n.order, unit.rows)[!is.na(match(prepped$n.order, unit.rows))]
    imps <- imps[order(outoforder),]
    
    means <- rowMeans(imps)

    uppers <- apply(imps, 1, quantile, probs=(conf + (1 - conf)/2))
    lowers <- apply(imps, 1, quantile, probs=(1-conf)/2)
  } else {
    means <- data[unit.rows, var]
    uppers <- lowers <- means
  }
  

  if (missing(pch)) pch <- 19
  if (missing(xlab)) xlab <- "time"
  if (missing(ylab)) ylab <- names(data)[var]
  if (missing(main)) main <- cs
  if (missing(xlim)) xlim <- range(time)
  if (missing(ylim)) ylim <- range(c(uppers,lowers,means))
  
  cols <- ifelse(miss, misscol, obscol)
  if (frontend) {
    if (.Platform$OS.type == "windows") {
      windows()
    } else {
      x11()      
    }
  }
  plot(x = time, y = means, col = cols, pch = pch,ylim = ylim, xlim = xlim,
       ylab = ylab, xlab = xlab, main = main, ...)
  segments(x0 = time, x1 = time, y0 = lowers, y1 = uppers, col = cols, ...)

  oiDetect <- (sum(output$missMatrix[unit.rows,var]) +
               sum(!is.na(data[unit.rows, var]))) > length(unit.rows)
  if (oiDetect) {
    points(x = time, y = data[unit.rows, var], pch = pch, col = obscol)
  }
    
  invisible(imps)
  
}
