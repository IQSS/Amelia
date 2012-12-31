#include "em.h"
#include <RcppArmadillo.h>


using namespace Rcpp ;

SEXP emcore(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP ivec, SEXP thetas, SEXP tols, SEXP emburns, SEXP p2ss, SEXP empris, SEXP autos, SEXP alls, SEXP prs){

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

  NumericVector emprir(empris);
  // NumericVector frontend(fends);
  NumericVector allthetas(alls);
  NumericVector autopri(autos);
  
  int p2s = p2sr(0), empri = emprir(0);
  int n = xr.nrow(), k = xr.ncol();
  int const AMn = n;
  int npatt = orr.nrow();
  int cvalue = 1;

  arma::mat x(xr.begin(), n, k, false);
  arma::mat thetaold(thetar.begin(), k + 1, k + 1, false);
  arma::mat AMr1(AMr1r.begin(), n, k, false);
  arma::mat obsmat(orr.begin(), npatt, k, false);
  arma::mat mismat(mr.begin(), npatt, k, false);
  arma::vec ii(ir.begin(), n, false);
  //Rcpp::Rcout << "Set up arma things. "  << std::endl;

  // Bring out your priors.
  NumericMatrix prr;
  int npr, knr;
  arma::mat priors;
  if (!Rf_isNull(prs)) {
    prr = NumericMatrix(prs);
    npr = prr.nrow();
    knr = prr.ncol();
    priors = arma::mat(prr.begin(), npr, knr, false); 
  }

  int count = 0;
  int is, isp;
  
  int nparam = arma::accu(arma::find(arma::trimatu(thetaold)));
  
  arma::uvec upperpos = arma::find(arma::trimatu(arma::abs(arma::randu<arma::mat>(k+1,k+1))));
  arma::mat xplay = arma::zeros<arma::mat>(AMn,k);
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
  arma::mat thetaHolder(upperpos.n_elem,1);
  thetaHolder.col(0) = thetaold.elem(upperpos);
  
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
  //  if (empri > 0) {
  arma::mat hold = empri * arma::eye(k,k);
  arma::mat simple(k,k);
//}

  if (p2s > 0) Rcpp::Rcout << std::endl;
  //Rcpp::Rcout << "Starting loop. "  << std::endl;
while ( ( (cvalue > 0) | (count < emburn(0)) )  & ( (count < emburn(1)) | (emburn(1) < 1))) {
    count++;
    hmcv.zeros(k,k);
    music.zeros(k);
    xplay.zeros(AMn,k);

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
    if (Rf_isNull(prs)) {
      for (ss = st; ss < obsmat.n_rows; ss++) {
        
        is = ii(ss)-1;
        isp = ii(ss+1)-2;
        
        theta = thetaold;
        sweeppos.zeros();
        sweeppos(arma::span(1,k)) = arma::trans(obsmat.row(ss));
        
        sweep(theta, sweeppos);
        
        imputations.zeros();
        imputations.set_size(isp - is, k);
        
        imputations = x.rows(is, isp) * theta(arma::span(1,k), arma::span(1,k));
        imputations.each_row() += theta(0, arma::span(1,k));
        imputations = AMr1.rows(is, isp) % imputations;

        xplay.rows(is, isp) = x.rows(is, isp) + imputations;
        
        mispos = arma::find(mismat.row(ss));
        hmcv(mispos, mispos) += (1+ isp - is) *  theta(mispos+1, mispos+1);
        
        
        
        
      }
    } else {
      for (ss = st; ss < obsmat.n_rows; ss++) {
        is = ii(ss)-1;
        isp = ii(ss+1)-2;
        
        theta = thetaold;
        sweeppos.zeros();
        sweeppos(arma::span(1,k)) = arma::trans(obsmat.row(ss));
        
        sweep(theta, sweeppos);

        imputations.zeros();
        imputations.set_size(isp - is, k);
        
        imputations = x.rows(is, isp) * theta(arma::span(1,k), arma::span(1,k));
        imputations.each_row() += theta(0, arma::span(1,k));
        imputations = AMr1.rows(is, isp) % imputations;
        
        mispos = arma::find(mismat.row(ss));
        arma::mat solveSigma = arma::inv(theta(mispos + 1, mispos + 1));
        arma::mat diagLambda = arma::zeros<arma::mat>(mispos.n_elem, mispos.n_elem);
        for (int p = 0; p <= isp-is; p++) {
          arma::uvec prRow = arma::find(priors.col(0) == p + is + 1);
          if (prRow.n_elem > 0) {
            arma::uvec pu(1);
            pu(0) = p;
            arma::mat thisPrior = priors.rows(prRow);
            arma::uvec theseCols = arma::conv_to<arma::uvec>::from(thisPrior.col(1)-1);
            arma::vec prHolder = arma::zeros<arma::vec>(k);
            prHolder.elem(theseCols) = thisPrior.col(3);
            diagLambda.diag() = prHolder.elem(mispos);
            arma::mat wvar = arma::inv(diagLambda + solveSigma);
            prHolder.elem(theseCols) = thisPrior.col(2);
            arma::mat muMiss = wvar * (prHolder.elem(mispos) + solveSigma * imputations(pu, mispos));
            imputations(pu, mispos) = muMiss;
            hmcv(mispos, mispos) +=  wvar;
          } else {
            hmcv(mispos, mispos) += theta(mispos + 1, mispos + 1);
          }
          
        }
        xplay.rows(is, isp) = x.rows(is, isp) + imputations;
      }

    }
    
    hmcv += arma::trans(xplay) * xplay;
    music += arma::trans(arma::sum(xplay));
    if (empri > 0) {
      simple = (music * arma::trans(music))/AMn;
      hmcv = (( (double)AMn/(AMn+empri+k+2)) * (hmcv - simple + hold)) + simple;
    } 

    thetanew(0,0) = AMn;
    thetanew(0, arma::span(1,k)) = arma::trans(music);
    thetanew(arma::span(1,k), 0) = music;
    thetanew(arma::span(1,k), arma::span(1,k)) = hmcv;
    thetanew = thetanew/AMn;

    sweeppos.zeros();
    sweeppos(0) = 1;
    sweep(thetanew, sweeppos);
    theta = arma::abs(thetanew - thetaold);
    thetaleft = arma::find(arma::trimatu(theta) > tol(0));
    cvalue = thetaleft.n_elem;
    thetaold = thetanew;

    if (cvalue > iterHist(count-1,0) & count > 20) {
      monoFlag = 1;
      if (autopri(0) > 0) {
        if (arma::accu(iterHist(arma::span(count - 20, count - 1), 2)) > 3) {
          if (empri < (autopri(0) * (double)n)) {
            empri = empri + 0.01 * (double)n;
          }
        }
      }
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
    if (allthetas(0) == 1) {   
      thetaHolder.resize(thetaHolder.n_rows, thetaHolder.n_cols + 1);
      thetaHolder.col(count) = thetaold.elem(upperpos);
    }
  }
  iterHist.shed_row(0);
  
  if (p2s > 0) Rcpp::Rcout << std::endl;
  List z;
  if (allthetas(0) == 1) {
    thetaHolder.shed_row(0);
    z = List::create(Rcpp::Named("thetanew") = thetaHolder, 
                     Rcpp::Named("iter.hist") = iterHist);
  } else {
    z = List::create(Rcpp::Named("theta") = thetaold, 
                     Rcpp::Named("iter.hist") = iterHist);
  }
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
  int p = g.n_rows;
  arma::uvec k = arma::find(m);
  arma::uvec kcompl = arma::find(1-m);   
  arma::mat h(g);
  if (k.n_elem == p) {
    g = -arma::inv(g);
  } else {
    arma::vec etest = arma::eig_sym(g(k,k));
    if (arma::accu(etest <= sqrt(arma::datum::eps))) {
      h(k,k) = arma::pinv(g(k,k), sqrt(arma::datum::eps));
    } else {
      h(k,k) = arma::inv(g(k,k));
    }
    h(k,kcompl) = h(k,k) * g(k,kcompl);
    h(kcompl,k) = arma::trans(h(k,kcompl));
    h(kcompl, kcompl) = g(kcompl, kcompl) - (g(kcompl, k)* h(k,k) * g(k,kcompl));
    h(k,k) = -h(k,k);
    g = h;
  }

}

SEXP ameliaImpute(SEXP xs, SEXP AMr1s, SEXP os, SEXP ms, SEXP ivec, SEXP thetas,  SEXP prs, SEXP bdss, SEXP maxres){

  //, SEXP p2ss, SEXP prs, SEXP empris, SEXP fends, SEXP alls, SEXP autos, SEXP emburns//

  NumericMatrix xr(xs);
  NumericMatrix thetar(thetas);
  NumericMatrix AMr1r(AMr1s);
  NumericMatrix orr(os);
  NumericMatrix mr(ms);
  NumericVector ir(ivec);
  NumericMatrix bdr;
  NumericVector maxrr;
  int maxsamples;

  int n = xr.nrow(), k = xr.ncol();
  int const AMn = n;
  int npatt = orr.nrow();
  int cvalue = 1;

  arma::mat x(xr.begin(), n, k, false);
  arma::mat thetaold(thetar.begin(), k + 1, k + 1, false);
  arma::mat AMr1(AMr1r.begin(), n, k, false);
  arma::mat obsmat(orr.begin(), npatt, k, false);
  arma::mat mismat(mr.begin(), npatt, k, false);
  arma::vec ii(ir.begin(), n, false);
  //Rcpp::Rcout << "Set up arma things. "  << std::endl;

  // Bring out your priors.
  NumericMatrix prr;
  int npr, knr;
  arma::mat priors;
  if (!Rf_isNull(prs)) {
    prr = NumericMatrix(prs);
    npr = prr.nrow();
    knr = prr.ncol();
    priors = arma::mat(prr.begin(), npr, knr, false); 
  }

  arma::mat bounds;
  if (!Rf_isNull(bdss)) {
    bdr = NumericMatrix(bdss);
    bounds = arma::mat(bdr.begin(), bdr.nrow(), bdr.ncol(), false);
    maxrr = NumericVector(maxres);
    maxsamples = maxrr(0);
  }

  int is, isp;
  
  int nparam = arma::accu(arma::find(arma::trimatu(thetaold)));
  
  arma::uvec upperpos = arma::find(arma::trimatu(arma::abs(arma::randu<arma::mat>(k+1,k+1))));
  arma::mat xplay = arma::zeros<arma::mat>(AMn,k);
  arma::mat imputations(2,k);
  arma::mat theta(k+1, k+1);
  arma::mat junk(2,k);
  arma::mat Ci(k, k);
  arma::vec sweeppos(k+1);
  arma::uvec mispos;
  
  sweeppos.zeros();
  int st, ss;
  if (arma::accu(mismat.row(0)) == 0) {
    st = 1;
  } else {
    st = 0;
  }

  //Rcpp::Rcout << "Starting loop. "  << std::endl;

  
  if (st == 1) {
    xplay.rows(0,ii(1)-2) = x.rows(0,ii(1)-2);
  }    
  if (Rf_isNull(prs)) {
    for (ss = st; ss < obsmat.n_rows; ss++) {
      
      is = ii(ss)-1;
      isp = ii(ss+1)-2;
      
      theta = thetaold;
      sweeppos.zeros();
      sweeppos(arma::span(1,k)) = arma::trans(obsmat.row(ss));
      
      sweep(theta, sweeppos);

      mispos = arma::find(mismat.row(ss));
      Ci.zeros(k, k);
      Ci(mispos, mispos) = chol(theta(mispos+1, mispos + 1));       
      junk = Rcpp::rnorm((isp - is + 1)* k, 0, 1);
      junk.reshape(isp - is +1, k);
      junk = junk * Ci;

      imputations.zeros();
      imputations.set_size(isp - is, k);      
      imputations = x.rows(is, isp) * theta(arma::span(1,k), arma::span(1,k));
      imputations.each_row() += theta(0, arma::span(1,k));
      imputations = AMr1.rows(is, isp) % imputations;

      if (Rf_isNull(bdss)) {
        xplay.rows(is, isp) = x.rows(is, isp) + imputations + junk;
      } else {
        xplay.rows(is, isp) = resampler(x.rows(is, isp), Ci, imputations, mispos, bounds, maxsamples);
      }
      
    }
  } else {
    for (ss = st; ss < obsmat.n_rows; ss++) {
      is = ii(ss)-1;
      isp = ii(ss+1)-2;
      
      theta = thetaold;
      sweeppos.zeros();
      sweeppos(arma::span(1,k)) = arma::trans(obsmat.row(ss));
      
      sweep(theta, sweeppos);
      junk.zeros(isp - is + 1, k);
      imputations.zeros();
      imputations.set_size(isp - is, k);
      
      imputations = x.rows(is, isp) * theta(arma::span(1,k), arma::span(1,k));
      imputations.each_row() += theta(0, arma::span(1,k));
      imputations = AMr1.rows(is, isp) % imputations;
      
      mispos = arma::find(mismat.row(ss));
      arma::mat solveSigma = arma::inv(theta(mispos + 1, mispos + 1));
      arma::mat diagLambda = arma::zeros<arma::mat>(mispos.n_elem, mispos.n_elem);
      for (int p = 0; p <= isp-is; p++) {
        arma::uvec prRow = arma::find(priors.col(0) == p + is + 1);
        Ci.zeros(k,k);
        if (prRow.n_elem > 0) {
          arma::uvec pu(1);
          pu(0) = p;
          arma::mat thisPrior = priors.rows(prRow);
          arma::uvec theseCols = arma::conv_to<arma::uvec>::from(thisPrior.col(1)-1);
          arma::vec prHolder = arma::zeros<arma::vec>(k);
          prHolder.elem(theseCols) = thisPrior.col(3);
          diagLambda.diag() = prHolder.elem(mispos);
          arma::mat wvar = arma::inv(diagLambda + solveSigma);
          prHolder.elem(theseCols) = thisPrior.col(2);
          arma::mat muMiss = wvar * (prHolder.elem(mispos) + solveSigma * imputations(pu, mispos));
          imputations(pu, mispos) = muMiss;
          Ci(mispos, mispos) = wvar;
        } else {
          Ci(mispos, mispos) = theta(mispos + 1, mispos + 1);
        }
        junk.row(p) = arma::rowvec(rnorm(k,0,1).begin(), k);
        junk.row(p) = junk.row(p) * Ci;
        if (Rf_isNull(bdss)) {
          xplay.row(is + p) = x.row(is + p) + imputations.row(p) + junk.row(p);
        } else {
          xplay.row(is + p) = resampler(x.row(is + p), Ci, imputations.row(p), mispos, bounds, maxsamples);
        }

      }
    }
    
  }

      
  return wrap(xplay);
}

arma::mat resampler(arma::mat x, arma::mat ci, arma::mat imps, arma::uvec mss,
                    arma::mat bounds, int maxsample) {
  int nss = x.n_rows, k = x.n_cols;

  arma::mat ub(nss, k);
  arma::mat lb(nss, k);
  arma::umat utest;
  arma::umat ltest;
  ub.fill(arma::datum::inf);
  lb.fill(-arma::datum::inf);
  arma::mat xp = arma::zeros<arma::mat>(nss, k);
  
  arma::mat junk = rnorm(nss * k, 0, 1);
  junk.reshape(nss, k);
  junk = junk * ci;
  int nb = 0, bdvar;
  for (int j = 0; j < bounds.n_rows; j++) {
    bdvar = (int) bounds(j,0) - 1;
    if (arma::accu(mss == bdvar)) {
      nb++;
      lb.col(bdvar) = arma::ones<arma::colvec>(nss) * bounds(j,1);
      ub.col(bdvar) = arma::ones<arma::colvec>(nss) * bounds(j,2);
    }
  }
  
  if (nb == 0) {
    return x;
  }

  int samp = 0;
  arma::colvec done = arma::zeros<arma::colvec>(nss);
  arma::colvec left = arma::ones<arma::colvec>(nss);
  arma::uvec finished;
  while ((arma::accu(left) > 0) & (samp < maxsample)) {
    samp++;
    utest = (imps + junk) > ub;
    ltest = (imps + junk) < lb;

    done += left % (arma::sum(utest + ltest, 1) == 0);
    finished = arma::find(left % (arma::sum(utest + ltest, 1) == 0));
    left -= left % (arma::sum(utest + ltest,  1) == 0);
    

    ub.rows(finished).fill(arma::datum::inf);
    lb.rows(finished).fill(-arma::datum::inf);
    xp.rows(finished) = imps.rows(finished) + junk.rows(finished);
    
    junk = rnorm(nss * k, 0, 1);
    junk.reshape(nss, k);
    junk = junk * ci;

  }
  
  if (arma::accu(left) > 0) {
    utest = (imps + junk) > ub;
    ltest = (imps + junk) < lb;
    arma::uvec ufails = arma::find(utest);
    arma::uvec lfails = arma::find(sum(ltest, 1) > 0);
    xp.elem(ufails) = ub.elem(ufails);
    xp.elem(lfails) = lb.elem(lfails);
  }
  
  return xp;
  
}
