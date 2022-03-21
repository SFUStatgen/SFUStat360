#include <Rcpp.h>
using namespace Rcpp;

double target(double x) {
  if(x<0){ 
    return 0;
  } else { 
    return exp(-x);
  }
}

// [[Rcpp::export]]
NumericVector easyMCMC_C(int niter, double startval, double proposalsd){
  NumericVector x(niter);
  double currentx, proposedx, A;
  int i;
  x[0] = startval;   // zero-based indexing  
  for(i=1; i<niter; i++){
    currentx = x[i-1];
    proposedx = rnorm(1,currentx,proposalsd)[0]; // note [0] at end of NumericVector output by rnorm
    A = target(proposedx)/target(currentx);
    if(runif(1)[0] < A){ // note [0] after runif(1)
      x[i] = proposedx; //accept move with probability min(1,A)
    } else {
      x[i] = currentx; //otherwise "reject" move, and stay where we are
    }
  }
  return x;
}

