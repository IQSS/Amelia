#include "em.h"
#include <RcppArmadillo.h>

using namespace Rcpp ;

SEXP emcore(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP ivec, SEXP thetas, SEXP tols, SEXP emburns, SEXP p2ss, SEXP empris){

  //, SEXP p2ss, SEXP prs, SEXP empris, SEXP fends, SEXP alls, SEXP autos, SEXP emburns//

  NumericMatrix xr(xs);
  NumericMatrix thetar(thetas);
  NumericVector tol(tols);
  NumericMatrix AMr1r(AMr1s);
  NumericMatrix orr(os);
  NumericMatrix mr(ms);
  NumericVector ir(ivec);
  NumericVector emburn(emburns);
  NumericVector p2sr(p2ss);
  // NumericMatrix prr(prs);
  NumericVector empri(empris);
  // NumericVector frontend(fends);
  // NumericVector allthetas(alls);
  // NumericVector autopri(autos);
  
  int p2s = p2sr(0);
  int n = xr.nrow(), k = xr.ncol();
  // int npr = prr.nrow(), knr = prr.ncol();
  int npatt = orr.nrow();
  int cvalue = 1;
  arma::mat x(xr.begin(), n, k, false);
  arma::mat thetaold(thetar.begin(), k + 1, k + 1, false);
  //arma::mat priors(prr.begin(), npr, knr, false);
  arma::mat AMr1(AMr1r.begin(), n, k, false);
  arma::mat obsmat(orr.begin(), npatt, k, false);
  arma::mat mismat(mr.begin(), npatt, k, false);
  arma::vec ii(ir.begin(), n, false);
  //Rcpp::Rcout << "Set up arma things. "  << std::endl;
  int count = 0;
  int is, isp;
  arma::mat xplay(n,k);
  arma::mat hmcv(k,k);
  arma::mat imputations(2,k);
  arma::vec music(k);
  arma::mat thetanew(k+1, k+1);
  arma::mat theta(k+1, k+1);
  arma::vec sweeppos(k+1);
  arma::uvec mispos;
  arma::uvec thetaleft;
  arma::vec etest;
  arma::mat iterHist(1,3);
  iterHist.zeros();
  sweeppos.zeros();
  hmcv.zeros();
  music.zeros();
  int st, ss, singFlag, monoFlag;
  if (arma::accu(mismat.row(0)) == 0) {
    st = 1;
  } else {
    st = 0;
  }
  if (p2s > 0) Rcpp::Rcout << std::endl;
  //Rcpp::Rcout << "Starting loop. "  << std::endl;
  while ( (cvalue > 0 | count < emburn(0)) & (count < emburn(1) | emburn(1) < 1)) {
    count++;
    hmcv.zeros();
    music.zeros();
    xplay.zeros();

    if (p2s > 0) {
      if (count < 10) {
        Rcpp::Rcout << "  " << count;
      } else {
        Rcpp::Rcout << " " << count;
      }
      if (count % 20 == 0) {
        Rcpp::Rcout << std::endl;
      }
    }
    if (st == 1) {
      xplay.rows(0,ii(1)-2) = x.rows(0,ii(1)-2);
    }
    
    for (ss = st; ss < obsmat.n_rows; ss++) {
      is = ii(ss)-1;
      isp = ii(ss+1)-2;
      
      theta = thetaold;
      sweeppos.zeros();
      sweeppos(arma::span(1,k)) = arma::trans(obsmat.row(ss));
      //Rcpp::Rcout << std::endl << theta.row(1);
      sweep(theta, sweeppos);
      //Rcpp::Rcout << std::endl << theta.row(1);
      imputations.zeros();
      imputations.set_size(isp - is, k);
      
      imputations = x.rows(is, isp) * theta(arma::span(1,k), arma::span(1,k));
      imputations.each_row() += theta(0, arma::span(1,k));
      imputations = AMr1.rows(is, isp) % imputations;
      
      xplay.rows(is, isp) = x.rows(is, isp) + imputations;
      
      mispos = arma::find(mismat.row(ss));
      hmcv(mispos, mispos) += (1+ isp - is) *  theta(mispos+1, mispos+1);
      

                                                   
                                          
    }
    hmcv += arma::trans(xplay) * xplay;
    music += arma::trans(arma::sum(xplay));
    if (empri(0) > 0) {
      
    }
    //Rcpp::Rcout << "xplay: " << music << std::endl;
    thetanew(0,0) = n;
    thetanew(0, arma::span(1,k)) = arma::trans(music);
    thetanew(arma::span(1,k), 0) = music;
    thetanew(arma::span(1,k), arma::span(1,k)) = hmcv;
    
    thetanew = thetanew/n;

    sweeppos.zeros();
    sweeppos(0) = 1;
    sweep(thetanew, sweeppos);
    theta = arma::abs(thetanew - thetaold);
    thetaleft = arma::find(arma::trimatu(theta) > tol(0));
    cvalue = thetaleft.n_elem;
    thetaold = thetanew;

    if (cvalue > iterHist(count-1,0) & count > 20) {
      monoFlag = 1;
    } else {
      monoFlag = 0;
    }
    etest = arma::eig_sym(thetaold(arma::span(1,k), arma::span(1,k)));
    if (arma::accu(etest <= 0)) {
      singFlag = 1;
    } else {
      singFlag = 0;
    }
    if (p2s > 1) {
      Rcpp::Rcout << "(" << cvalue << ")";
      if (monoFlag == 1) {
        Rcpp::Rcout << "*";
      }
      if (singFlag == 1) {
        Rcpp::Rcout << "!";
      }
    }
    iterHist.resize(iterHist.n_rows+1, iterHist.n_cols);
    iterHist(count, 0) = cvalue;
    iterHist(count, 1) = singFlag;
    iterHist(count, 2) = monoFlag;
  
  }
  iterHist.shed_row(0);

  if (p2s > 0) Rcpp::Rcout << std::endl;
  List z = List::create(Rcpp::Named("theta") = thetaold, 
                        Rcpp::Named("iter.hist") = iterHist) ;
  return z ;
}

// void sweep(arma::mat& g, arma::vec m) {
//   int p = g.n_rows, h, j, i;
//   arma::uvec k = arma::find(m);    

//   if (k.n_elem == p) {
//     g = -arma::inv(g);
//   } else {
//     for (h = 0; h < k.n_rows; h++) {
//       for (j = 0; j < p; j++) {
//         for (i = 0; i <= j; i++) {
//           if (i == k(h)) {
//             if (j == k(h)) {
//               g(i,j) = -1/g(i,j);
//               //Rcpp::Rcout << k(h) << ": " << "(i,j): (" <<i<<", "<<j<<"): " << g(i,j) << std::endl;

//             } else {
//               //Rcpp::Rcout << k(h) << ": " << "(i,j): (" <<i<<", "<<j<<"): " << g(i,j)<<"\t" << g(i,i) << std::endl;
//               g(i,j) = g(i,j)/g(i,i);
//             }
//           } else {
//             //Rcpp::Rcout << k(h) << ": " << "(i,j): (" <<i<<", "<<j<<"): " << g(i,j) << std::endl;
//             g(i,j) = g(i,j) - g(i, k(h)) * g(k(h), j)/g(k(h), k(h));
//           }
//         }
//       }
//     }
//     g = arma::symmatu(g);
//   }

// }

void sweep(arma::mat& g, arma::vec m) {
  int p = g.n_rows, j, i;
  arma::uvec k = arma::find(m);
  arma::uvec kcompl = arma::find(1-m);   
  arma::mat h(g);
  if (k.n_elem == p) {
    g = -arma::inv(g);
  } else {
    h(k,k) = arma::inv(g(k,k));
    h(k,kcompl) = h(k,k) * g(k,kcompl);
    h(kcompl,k) = arma::trans(h(k,kcompl));
    h(kcompl, kcompl) = g(kcompl, kcompl) - (g(kcompl, k)* h(k,k) * g(k,kcompl));
    h(k,k) = -h(k,k);
    g = h;
  }

}
