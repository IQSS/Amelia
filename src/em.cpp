#include "em.h"
#include <RcppArmadillo.h>

using namespace Rcpp ;

SEXP emarch(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP is, SEXP thetas, SEXP tols){

  //, SEXP p2ss, SEXP prs, SEXP empris, SEXP fends, SEXP alls, SEXP autos, SEXP emburns//

  NumericMatrix xr(xs);
  NumericVector p2s(p2ss);
  NumericMatrix thetar(thetas);
  NumericVector tol(tols);
  NumericMatrix AMr1r(AMr1s);
  NumericMatrix orr(os);
  NumericMatrix mr(ms);
  NumericVector ir(is);

  // NumericMatrix prr(prs);
  // NumericVextor empri(empris);
  // NumericVector frontend(fends);
  // NumericVector allthetas(alls);
  // NumericVector autopri(autos);
  // NumericVector emburn(emburns);

  int n = xr.nrow(), k = xr.ncol();
  int npr = prr.nrow(), knr = prr.ncol();
  int npatt = orr.nrow();
  double cvalue = 1+tol;
 
  arma::mat X(xr.begin(), n, k, false);
  arma::mat thetaold(thetar.begin(), k + 1, k + 1, false);
  //arma::mat priors(prr.begin(), npr, knr, false);
  arma::mat AMr1(AMr1r.begin(), n, k, false);
  arma::mat obsmat(orr.begin(), npatt, k, false);
  arma::mat mismat(mr.begin(), npatt, k, false);
  arma::vec ii(ir.begin(), n, false);

  int count = 0;
  int is, isp;
  arma::mat(k,k) hmcv;
  arma::mat(1,k) imputations;
  arma::mat(1,k) xplay;
  arma::vec(k) music;
  arma::mat thetanew(k+1, k+1);
  arma::mat theta(k+1, k+1);
  arma::vec(k+1) sweeppos;
  sweeppos.zeros();
  sweeppos(0) = 1;
  hmcv.zeros();
  music.zeros();
  int st;
  if (arma::accu(mismat.row(0)) == 0) {
    st = 1;
  } else {
    st = 0;
  }
  while (cvalue > 0) {
    count++;
    hmcv.zeros();
    music.zeros();
    
    if (st == 1) {
      hmcv += arma::trans(x.rows(0,i(0)-1)) * x.rows(0, i(0)-1)
    }
    
    for (ss = st; ss < ii.n_rows -1; ss++) {
      is = i(ss)-1;
      isp = i(ss+1)-2;
      theta = sweep(thetaold, obsmat.row(ss));
      imputations.zeros();
      imputations.set_size(isp - is, k);
      
      imputations = x.rows(is, isp) * theta.submat(span(1,k), span(1,k));
      imputations.each_row() += theta.submat(0, span(1,k));
      imputations = AMr1.rows(is, isp) % imputations;

      xplay.zeros();
      xplay.set_size(isp - is, k);
      xplay = x.rows(is, isp) + imputations;
      
      hmcv += (1 + isp - is) % theta.submat(arma::find(mismat) + 1, arma::find(mismat) + 1);
      hmcv += arma::trans(xplay) * xplay;
      music += arma::sum(xplay);
                                                   
                                          
    }
    thetanew(0,0) = n;
    thetanew(0, span(1,k)) = music;
    thetanew(span(1,k), 0) = music;
    thetanew(span(1,k), span(1,k)) = hmcv;
    thetanew = thetanew % (1/n);
    thetanew = sweep(thetanew, sweepos);
    cvalue = arma:accu(arma::trimatu(arma::abs(thetanew-thetaold) > tol));
    thetaold = thetanew;
  }
  NumericMatrix iterHist = NumericMatrix(count, 3);
  List z = List::create(thetaold, iterHist) ;
  return z ;
}

arma::mat sweep(arma::mat g, arma::vec m) {
  int p = g.n_rows, rowsm = sum(m), h, j, i;
  if (rowsm == p) {
    h = -arma::inv(g);
  } else {
    arma::uvec k = arma::find(m);    
    for (h = 0; h < k.n_rows; h++) {
      for (j = 0; j < p; j++) {
        for (i = 0; i <= j; i++) {
          if (i == k[h]) {
            if (j == k[h]) {
              g(i,j) = -1/g(i,j);
            } else {
              g(i,j) = g(i,j)/g(i,i);
            }
          }
          g(i,j) = g(i,j) - g(i, k[h])*g(k[h], j)/g(k[h], k[h]);
        }
      }
    }
    g = arma::symmatu(g);
  }
  return g;
}
