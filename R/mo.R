##
##  mo.R - Functions to implement multiple overimputation.
##



moPrep <- function(x, formula, subset, error.proportion, gold.standard=!missing(subset), error.sd) {
  UseMethod("moPrep",x)
}

moPrep.molist <- function(x, formula, subset, error.proportion, gold.standard=FALSE, error.sd) {
  m <- match.call()
  m$x <- x$data
  m[[1]] <- as.name("moPrep.default")
  res <- eval(m, sys.frame(sys.parent()))

  x$priors <- rbind(x$priors, res$priors)
  x$overimp <- rbind(x$overimp, res$overimp)
  return(x)
}

moPrep.default <- function(x, formula, subset, error.proportion, gold.standard=!missing(subset), error.sd) {

  if (!missing(error.proportion) &&
      !(length(error.proportion) %in% c(1,nrow(x)))) {
    stop("The error.proportion arugment must be of length 1 or the number of rows of the data.")
  }

  if (!missing(error.sd) &&
      !(length(error.sd) %in% c(1,nrow(x)))) {
    stop("The error.sd arugment must be of length 1 or the number of rows of the data.")
  }

  if (!missing(error.proportion) & !missing(error.sd)) {
    stop("error.proportion and error.sd cannot be set at the same time.")
  }
  ## parse the formula
  target.name <- formula[[2]]
  pars <- formula[[3]]
  vnames <- all.vars(formula, unique = FALSE)

  if ("|" %in% all.names(formula)) {
    proxyname <- vnames[length(vnames)]
    meanpos <- length(vnames)-1
  } else {
    meanpos <- length(vnames)
  }
  if (!exists("proxyname") && missing(error.proportion) && !gold.standard && missing(error.sd)) {
    stop("Need to specify a proxy, an error proportion, an error variance, or gold-standard data.")
  }

  proxysplit <- strsplit(deparse(formula), "\\|")[[1]]
  form <- formula(paste(proxysplit, collapse = "+"))


  m <- match.call()
  m[[1]] <- as.name("model.frame")
  m$formula <- form
  m$error.proportion <- NULL
  m$error.sd <- NULL
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
  if (!missing(error.sd)) {
    if (length(error.sd) == nrow(x)) {
      error.sd[eval(substitute(subset,x))]
      gs <- mf[error.sd == 0,]
      mf <- mf[error.sd != 0,]
    }
  } else {
    gs <- mf[0,]
  }

  if (ncol(mf) < meanpos)
    meanpos <- ncol(mf)
  prior.mean <- mf[,meanpos]
  var.mm <- var(mf[,1], na.rm=TRUE)

  if (!missing(error.proportion)) {
    prior.var <- var.mm*error.proportion
  }
  if (!missing(error.sd)) {
    prior.var <- error.sd^2
  }
  if (exists("proxyname")) {
    prior.var <- var.mm - cov(mf[,1],mf[,proxyname], use="complete.obs")
  }

  if (gold.standard && !is.null(m$subset)) {
    if (is.logical(subset)) {
      m$subset <- as.name(paste("!",m$subset,sep=""))
    } else if (is.character(subset)) {
      m$subset <- as.name(paste("setdiff(rownames(",x,"),",m$subset,")",sep=""))
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
  out$overimp <- cbind(rows, col)
  if (sum(out$priors[,4] <= 0) > 0) {
    out$priors <- out$priors[out$priors[,4] > 0,]
    warning("Some observations estimated with negative measurement error variance. Set to gold standard.")
  }
  out$priors[,4] <- sqrt(out$priors[,4])
  out$data <- substitute(x)
  class(out) <- c("molist","list")
  return(out)
}
