#include <Rcpp.h>
using namespace Rcpp;

// Mean percentage error 1/n sum_i (y_i-y^hat_i)/y_i

// [[Rcpp::export]]
double mpe(List mod) {
  if (!mod.inherits("lm")) stop("Input must be a linear model");
  // note use of .inherits method and stop() function
  
  // Extract list elements with as() 
  // Note: as() is a function "template" that works with generic types.
  //       Google "C++ function templates" for more info if interested
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  
  int n = resid.size();
  double err = 0;
  for(int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
}