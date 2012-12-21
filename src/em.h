#ifndef _Amelia_EMCORE_H
#define _Amelia_EMCORE_H
#include <Rcpp.h>
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
RcppExport SEXP emcore(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP is, SEXP p2ss, SEXP thetas, SEXP tols, SEXP prs, SEXP empris, SEXP fends, SEXP alls, SEXP autos, SEXP emburns) ;

#endif
