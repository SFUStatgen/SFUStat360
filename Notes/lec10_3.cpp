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
  // Extract list elements with as() 
  // Note: as() is a function "template" that works with generic types.
  //       Google "C++ function templates" for more info if interested
  return(lm(form,dat));
}