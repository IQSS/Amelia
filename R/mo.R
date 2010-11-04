##
##  mo.R - Functions to implement multiple overimputation.
##



moPrep <- function(x,...) {
  UseMethod("moPrep",x)
}

moPrep.molist <- function(x, formula, subset, error.proportion, gold.standard=FALSE) {
  m <- match.call()
  m$x <- x$data
  m[[1]] <- as.name("moPrep.default")
  res <- eval(m, sys.frame(sys.parent()))

  x$priors <- rbind(x$priors, res$priors)
  return(x)
}


## w1 ~ w1 | w2
moPrep.default <- function(x, formula, subset, error.proportion, gold.standard=!missing(subset)) {

  if (!missing(error.proportion) &&
      !(length(error.proportion) %in% c(1,nrow(x)))) {
    stop("The error.proportion arugment must be of length 1 or the number of rows of the data.")
  }

  ## parse the formula
  target.name <- formula[[2]]
  pars <- formula[[3]]

  proxysplit <- strsplit(deparse(formula),"\\|")[[1]]
  if (length(proxysplit) > 1) {
    proxyname <- proxysplit[[2]]
    meanpos <- length(all.vars(formula))-1
  } else {
    meanpos <- length(all.vars(formula))
  }
  if (!exists("proxyname") && missing(error.proportion) && !gold.standard) {
    stop("Need to specify a proxy, an error proportion, or gold-standard data.")
  }

  form <- formula(paste(proxysplit, collapse = "+"))


  m <- match.call()
  m[[1]] <- as.name("model.frame")
  m$error.proportion <- NULL
  m$gold.standard <- NULL
  m$data <- m$x
  m$x <- NULL
  mf <- eval(m, sys.frame(sys.parent()))
  if (!missing(error.proportion)) {
    if (length(error.proportion) == nrow(x)) {
      error.proportion[eval(substitute(subset,x))]
      gs <- mf[error.proportion == 0,]
      mf <- mf[error.proportion != 0,]
    }
  } else {
    gs <- mf[0,]
  }
  prior.mean <- mf[,meanpos]
  var.mm <- var(mf[,1], na.rm=TRUE)

  if (!missing(error.proportion)) {
    prior.var <- var.mm*error.proportion
  }
  if (exists("proxyname")) {
    prior.var <- var.mm - cov(mf[,1],mf[,proxyname], use="complete.obs")
  }
  
  if (gold.standard && !is.null(m$subset)) {
    if (is.logical(subset)) {
      m$subset <- as.name(paste("!",m$subset,sep=""))
    } else if (is.character(subset)) {
      m$subset <- as.name("setdiff(rownames(",x,"),",m$subset,")",sep="")
    } else {
      m$subset <- as.name(paste("-",m$subset,sep=""))
    }
    gs <- rbind(gs, eval(m, sys.frame(sys.parent())))
    var.gs <- var(gs[,1],na.rm=TRUE)
    prior.var <- var.mm - var.gs
  }

  col <- match(names(mf)[1], names(x))
  rows <- as.integer(rownames(mf))
  out <- list()
  out$priors <- cbind(rows,col,prior.mean, prior.var)
  if (sum(out$priors[,4] <= 0) > 0) {
    out$priors <- out$priors[out$priors[,4] > 0,]
    warning("Some observations estimated with negative measurement error variance. Set to gold standard.")
  }
  out$priors[,4] <- sqrt(out$priors[,4])
  out$data <- substitute(x)
  class(out) <- c("molist","list")
  return(out)
}
