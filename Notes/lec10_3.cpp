#include <Rcpp.h>
using namespace Rcpp;

// Example from https://gallery.rcpp.org/articles/r-function-from-c++/
// [[Rcpp::export]]
NumericVector callFunction(NumericVector x, Function f) {
  NumericVector res = f(x);
  return res;
}

//Example from text

// [[Rcpp::export]]
RObject callWithOne(Function f) {
  return f(1);
}

// [[Rcpp::export]]
RObject lm_in_C(RObject form, List dat,Function lm) {
  return(lm(form,dat));
}