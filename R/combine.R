est.matrix <- function(x, name) {
  vals <- lapply(x, function(z) z[[name]])
  out <- do.call(cbind, vals)
  out
}


##' Combine results from statistical models run on multiply imputed
##' data sets using the so-called Rubin rules. 
##'
##' @title Combine results from analyses on imputed data sets
##' @param x List of output from statistical models estimated on
##' different imputed data sets, as outputted by \code{with(a.out,
##' expr)} where \code{a.out} is the output of a call to \code{amelia}.
##' @param conf.int Logical indicating if confidence intervals should
##' be computed for each quantity of interest (default is \code{FALSE}).
##' @param conf.level The confidence level to use for the confidence
##' interval  if \code{conf.level = TRUE}. Defaults to 0.95, which
##' corresponds to a 95 percent confidence interval.
##' @return Returns a \code{tibble} that contains:
##' \describe{
##' \item{term}{Name of the coefficient or parameter.}
##' \item{estimate}{Estimate of the parameter, averagine across imputations.}
##' \item{std.error}{Standard error of the estimate, accounting for
##' imputation uncertainty.}
##' \item{statistic}{Value of the t-statistic for the estimated
##' parameter.}
##' \item{p.value}{p-value associated with the test of a null
##' hypothesis that the true coefficient is zero. Uses the
##' t-distribution with an imputation-adjusted degrees of freedom.}
##' \item{df}{Imputation-adjusted degrees of freedom for each
##' parameter.}
##' \item{r}{Relative increase in variance due to nonresponse.}
##' \item{miss.info}{Estimated fraction of missing information.}
##' \item{conf.low}{Lower bound of the estimated confidence interval.
##' Only present if \code{conf.int = TRUE}.}
##' \item{conf.high}{Upper bound of the estimated confidence interval.
##' Only present if \code{conf.int = TRUE}.}
##' }
##' @author Matt Blackwell
##'
##' @examples 
##' data(africa)
##' a.out <- amelia(x = africa, cs = "country", ts = "year", logs =
##' "gdp_pc")
##'
##' imp.mods <- with(a.out, lm(gdp_pc ~ infl + trade))
##'
##' mi.combine(imp.mods, conf.int = TRUE)
##' 
##' @export
mi.combine <- function(x, conf.int = FALSE, conf.level = 0.95) {
  if (requireNamespace("broom", quietly = TRUE)) {
    tidiers <- grep("^tidy\\.", ls(getNamespace("broom")), value = TRUE)
    tidiers <- gsub("tidy\\.", "", tidiers)    
  } else {
    rlang::abort("{broom} package required for mi.combine")
  }
  if (!(class(x[[1L]]) %in% tidiers)) {
    rlang::abort("analysis model does not have tidy() method.")
  }

  mi_tidy <- lapply(x, function(x) broom::tidy(x))
  m <- length(mi_tidy)

  out <- mi_tidy[[1L]]
  ests <- est.matrix(mi_tidy, "estimate")
  ses <- est.matrix(mi_tidy, "std.error")
  wi.var <- rowMeans(ses ^ 2)
  out$estimate <- rowMeans(ests)
  diffs <- sweep(ests, 1, rowMeans(ests))
  bw.var <- rowSums(diffs ^ 2) / (m - 1)

  out$std.error <- sqrt(wi.var + bw.var * (1 + 1 / m))
  r <- ((1 + 1 / m) * bw.var) / wi.var
  df <- (m - 1) * (1 + 1 / r) ^ 2
  miss.info <- (r + 2 / (df + 3)) / (r + 1)

  out$statistic <- out$estimate / out$std.error
  out$p.value <- 2 * stats::pt(out$statistic, df = df, lower.tail = FALSE)

  out$df <- df
  out$r <- r
  out$miss.info <- miss.info
  if (conf.int) {
    t.c <- stats::qt(1 - (1 - conf.level) / 2, df = df, lower.tail = FALSE)
    out$conf.low <- out$estimate - t.c * out$std.error
    out$conf.high <- out$estimate + t.c * out$std.error
  }
  out
}
