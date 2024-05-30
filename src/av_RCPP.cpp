#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' Matrix Multiplication : 3 components
//'
//' This function returns the matrix multiplication of 3 matrix
//' as for footprint calculation
//'
//' @param ef matrix
//' @param L matrix
//' @param FD matrix
//' @export
// [[Rcpp::export]]
arma::mat CFPcalculationRCPP(arma::mat ef, arma::mat L,arma::mat  FD){return ef*L*FD;}

 //' Matrix inverse 
 //'
 //' This function returns the inverse of a matrix 
 //'
 //' @param m1 matrix
 //' @export
 // [[Rcpp::export]]
arma::mat inversion_rcpp3(arma::mat m1){arma::mat m2 = inv(m1); return(m2);}

//' Matrix Multiplication : 2 components
//'
//' This function returns the matrix multiplication of 2 matrix
//'
//' @param matA matrix
//' @param matB matrix
//' @export
// [[Rcpp::export]]
arma::mat Mult2_rcpp3(arma::mat matA, arma::mat matB){return matA*matB;} 
  
  