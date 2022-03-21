#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector findInterval2(NumericVector x, NumericVector breaks) {
  IntegerVector out(x.size()); // initialize output vector

  NumericVector::iterator it, pos; // 
  IntegerVector::iterator out_it;

  for(it = x.begin(), out_it = out.begin(); it != x.end(); 
      ++it, ++out_it) {
    pos = std::upper_bound(breaks.begin(), breaks.end(), *it); 
    // `pos` is an iterator that points to the first element of `breaks` 
    // that is greater than the element of `x` that `it` points to; i.e.
    // the upper limit of the bin that *it is in 
    *out_it = std::distance(breaks.begin(), pos); 
    // calculate bin number with distance() and store in out via out_it
  }

  return out;
}
