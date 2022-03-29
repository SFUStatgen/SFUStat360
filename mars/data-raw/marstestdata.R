## code to prepare `marstestdata` dataset goes here
set.seed(123)
# Parameters
N <- 100; n <- 10
knot1 <- -0.5; knot2 <- 0; knot3 <- 0.5
beta1 <- 3; beta2 <- 5
error.SD <- 0.1
# Simulate x's
x <- matrix(rnorm(N*n),ncol=n)
# Simulate response variable
h <- function(x,s,t) {
  return(pmax(0,s*(x-t)))
}
lin.pred <- beta1*h(x[,1],+1,knot1) + beta2*h(x[,2],-1,knot2)*h(x[,1],+1,knot3)
y <- lin.pred + rnorm(n,sd=error.SD)
# Package response and explanatory variables as a data frame
marstestdata <- data.frame(cbind(y,x))
names(marstestdata) <- c("y",paste0("x",1:n))

usethis::use_data(marstestdata, overwrite = TRUE)
