# Generate test dataset with 10 covariates,
# n=100, piece-wise linear in x1 plus interaction between x1 and x2
# Simulation parameters
n <- 100
knot1 <- -0.5; knot2 <- 0; knot3 <- 0.5
beta1 <- 3; beta2 <- 5
error.SD <- 0.1


# Simulate covariates first x1,...,x10 from a multivariate normal distribution
# with the following covariance matrix. X1 and X3, X2 and X4 correlated
Sigma <- matrix(
  c(1.0,0.1,0.8,0.0,0,0,0,0,0,0,
    0.1,1.0,0.0,0.8,0,0,0,0,0,0,
    0.8,0.0,1.0,0.0,0,0,0,0,0,0,
    0.0,0.8,0.0,1.0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,1 ),byrow=TRUE,ncol=10
)
rmvnorm <- function(n,p=10,Sigma=diag(p)) {
  ss <- chol(Sigma)
  X <- matrix(rnorm(n*p),ncol=p)
  return(X%*%ss)
}
X <- rmvnorm(n,ncol(Sigma),Sigma)
round(var(X),2) # check

# Simulate response variable
h <- function(x,s,t) { 
  return(pmax(0,s*(x-t)))
}
beta1 <- 3; beta2 <- 5
lin.pred <- beta1*h(X[,1],+1,knot1) + beta2*h(X[,2],-1,knot2)*h(X[,1],+1,knot3)
y <- lin.pred + rnorm(n,sd=error.SD)
# Package response and explanatory variabless

dat <- data.frame(cbind(y,X)); names(dat) <- c("y",paste0("x",1:ncol(X)))

#----------------------------------------------------------------
#library(earth)
#ee <- earth(y~.,data=dat)
#summary(ee)
#X <- rmvnorm(n)
#lin.pred <- beta1*h(X[,1],+1,knot1) + beta2*h(X[,2],-1,knot2)
