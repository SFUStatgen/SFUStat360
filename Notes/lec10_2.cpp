#include <Rcpp.h>
using namespace Rcpp;

// Mean percentage error 1/n sum_i (y_i-y^hat_i)/y_i

// [[Rcpp::export]]
double mpe(List mod) {
  if (!mod.inherits("lm")) stop("Input must be a linear model");
  // note use of .inherits method of lm objects and stop() function
  
  // Extract list elements
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  // Note: as<NumericVector>() converts R objects (SEXP) to NumericVector
  //       See lecture notes on C++ template functions for more info
  
  int n = resid.size();
  double err = 0;
  for(int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
}