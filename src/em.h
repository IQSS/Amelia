#ifndef _Amelia_EMCORE_H
#define _Amelia_EMCORE_H
#include <RcppArmadillo.h>


/*
* note : RcppExport is an alias to `extern "C"` dened by Rcpp.
*
* It gives C calling convention to the rcpp hello world function so that
* it can be called from .Call in R. Otherwise, the C++ compiler mangles the
* name of the function and .Call can't nd it.
*
* It is only useful to use RcppExport when the function is intended to be called
* by .Call. See the thread http://thread.gmane.org/gmane.comp.lang.r.rcpp/649/focus=672
* on Rcpp-devel for a misuse of RcppExport
*/
RcppExport SEXP emcore(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP is, SEXP thetas, SEXP tols, SEXP emburn, SEXP p2ss, SEXP empris, SEXP autos, SEXP alls, SEXP prs) ;

void sweep(arma::mat& g, arma::vec m);

RcppExport SEXP ameliaImpute(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP ivec, SEXP thetas, SEXP prs, SEXP bdss, SEXP maxres);

arma::mat resampler(arma::mat x, arma::mat ci, arma::mat imps, arma::uvec mss,
                    arma::mat bounds, int maxsample); 

#endif
