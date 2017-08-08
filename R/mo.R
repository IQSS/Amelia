#' Prepare Multiple Overimputation Settings
#' @description 
#'  A function to generate priors for multiple overimputation of
#'  a variable measured with error.
#'
#'  @param {x}either a matrix, data.frame, or a object of class "molist"
#'    from a previous \code{moPrep} call. The first two derive the priors
#' from the data given, and the third will derive the priors from the
#'    first \code{moPrep} call and add them to the already defined
#'    priors.
#' @param {formula}a formula describing the nature of the measurement
#'    error for the variable. See "Details."
#' @param {subset}an optional vector specifying a subset of observations
#'    which possess measurement error.
#' @param {error.proportion}an optional vector specifying the fraction of
#'      the observed variance that is due to measurement error.
#' @param {gold.standard}a logical value indicating if values with no
#'    measurement error should be used to estimate the measurement error
#'    variance.
#' @param {error.sd}an optional vector specifying the standard error of
#'    the measurement error.
#'
#' \value{
#'  An instance of the S3 class "molist" with the following objects:
#' @param \item{priors}a four-column matrix of the multiple overimputation priors
#'      associated with the data. Each row of the matrix is \code{c(row,
#'                                                                  column, prior.mean, prior.sd)}. 
#' @param {overimp}a two-column matrix of cells to be overimputed. Each
#'    row of the matrix is of the form \code{c(row, column)}, which
#'    indicate the row and column of the cell to be overimputed.
#' @param {data}the object name of the matrix or data.frame to which
#'    priors refer.
#'  
#'  Note that \code{priors} and \code{overimp} might contain results from
#'  multiple calls to \code{moPrep}, not just the most recent. 
#'}
#' @details 
#'  This function generates priors for multiple overimputation of data
#'  measured with error. With the \code{formula} arugment, you can specify
#'  which variable has the error, what the mean of the latent data is, and
#'  if there are any other proxy measures of the mismeasured variable. The
#'  general syntax for the formula is: \code{errvar ~ mean | proxy},
#'  where \code{errvar} is the mismeasured variable, \code{mean} is a
#'  formula for the mean of the latent variable (usually just
#'                                               \code{errvar} itself), and \code{proxy} is a another mismeasurement of
#'  the same latent variable. The proxies are used to estimate the
#'  variance of the measurement error. 
#'  
#'  \code{subset} and \code{gold.standard} refer to the the rows of the
#'  data which are and are not measured with error. Gold-standard rows are
#'  used to estimate the variance of the
#'  measurement. error. \code{error.proportion} is used to estimate the
#'  variance of the measurement error by estimating the variance of the
#'  mismeasurement and taking the proportion assumed to be due to
#'  error. \code{error.sd} sets the standard error of the measurement
#'  error directly.
#'
#'@seealso 
#'  \code{\link{amelia}
#'  
#' @examples 
#'  data(africa)
#'  m.out <- moPrep(africa, trade ~ trade, error.proportion = 0.1)
#'  a.out <- amelia(m.out, ts = "year", cs = "country")
#'  plot(a.out)
#'  
#'  m.out <- moPrep(africa, trade ~ trade, error.sd = 1)
#'  a.out <- amelia(m.out, ts = "year", cs = "country")


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

  if (nrow(mf) == 0L)
    stop("0 cases to overimpute, check subset argument")
  if (!missing(error.proportion)) {
    if (length(error.proportion) == nrow(x)) {
      if (!missing(subset)) {
        error.proportion <- error.proportion[eval(substitute(subset,x))]
      }
      gs <- mf[error.proportion == 0, , drop = FALSE]
      mf <- mf[error.proportion != 0, , drop = FALSE]
    }
  } else if (!missing(error.sd)) {
    if (length(error.sd) == nrow(x)) {
      if (!missing(subset)) {
        error.sd <- error.sd[eval(substitute(subset,x))]
      }
      gs <- mf[error.sd == 0, , drop = FALSE]
      mf <- mf[error.sd != 0, , drop = FALSE]
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
    m$subset <- NULL
    mf.full <- eval(m, sys.frame(sys.parent()))
    gs2 <- mf.full[which(!(rownames(mf.full) %in% rownames(mf))), , drop = FALSE]
    gs <- rbind(gs, gs2)
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
