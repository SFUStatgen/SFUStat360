f <- function() {pause(0.1);g();h()} # pause() is from profvis
g <- function() {pause(0.1);h()}
h <- function() {pause(0.1)}

simdat <- function(n) {
  beta <- runif(1,min=-1,max=1)
  x <- rnorm(n)
  y <- beta * x + rnorm(n)
  data.frame(x=x,y=y)
}

bigd1 <- function(N=500,n=1000) {
  out <- NULL
  for(i in 1:N){
    ss <- simdat(n)
    out <- rbind(out,ss)
  }
  return(NULL)
}

bigd2 <- function(N=500,n=1000) {
  out <- NULL
  for(i in 1:N){
    ss <- as.matrix(simdat(n))
    out <- rbind(out,ss)
  }
  return(NULL)
}

layerInds <- function(layerNum,nrow) {
  ((layerNum-1)*nrow + 1):(layerNum*nrow) 
}

bigd3 <- function(N=500,n=1000) {
  out <- matrix(NA,nrow=N*n,ncol=2)
  for(i in 1:N){
    inds <- layerInds(layer=i,nrow=n)
    ss <- as.matrix(simdat(n))
    out[inds,] <- ss
  }
  return(NULL)
}
