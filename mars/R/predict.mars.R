# Methods: predict
predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) {
    X <- object$B
  }
  else {
    tt <- terms(object$formula)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)
    X <- split_X(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(X %*% beta)
}
split_X <- function(X,Bfuncs) {
  Xout <- matrix(0,nrow=nrow(X),ncol=length(Bfuncs))
  for(i in 1:length(Bfuncs)) {
    Xout[,i] <- one_split_X(X,Bfuncs[[i]])
  }
  Xout
}
one_split_X <- function(X,s) {
  Xout <- rep(1,nrow(X))
  if(nrow(s)>1) {
    for(i in 2:nrow(s)){
      Xout <- Xout * h(X[,s[i,"v"]],s[i,"s"],s[i,"t"])
    }
  }
  Xout
}
