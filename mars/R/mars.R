#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param formula an R formula
#' @param data a data frame containing your data
#' @param control  an object of class 'mars.control'
#' @param ... unused
#'
#' @return an object of class 'mars'
#' @export
#'
#' @examples
#'  mm <- mars(wage ~ age,dat=ISLR::Wage)
#' @import stats
#' @import ISLR
mars <- function(formula,data,control=NULL,...) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE] # ***
  x_names <- colnames(x)
  if(is.null(control)) control <- mars.control()
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B)) # notice -1 added
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}

fwd_stepwise <- function(y,x,control=mars.control()){
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors
  B <- init_B(N,control$Mmax)
  Bfuncs <- vector(mode="list",length=control$Mmax+1)
  #---------------------------------------------------
  for(i in 1:(control$Mmax/2)) {
    M <- 2*i-1
    if(control$trace) cat("M",M,"\n")
    lof_best <- Inf
    for(m in 1:M) { # choose a basis function to split
      #print(Bfuncs[[m]])
      svars <- setdiff(1:n,Bfuncs[[m]][,"v"]) # vars not in B_m; returns 1:n if Bfuncs[[m]] NULL
      if(control$trace) cat("M, m, svars",M,m,svars,"\n")
      for(v in svars){ # select a variable to split on
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,1:M], # ***SHD THIS BE 2:M, drop=FALSE
                             Btem1=B[,m]*h(x[,v],+1,t),
                             Btem2=B[,m]*h(x[,v],-1,t))

          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat,control) # *** SHD BE y ~ . -1
          if(lof < lof_best) {
            lof_best <- lof
            # m_best <- m; v_best <- v; t_best <- t
            split_best <- c(m=m,v=v,t=t)
          }
        }
      }
    }
    m <- split_best["m"]; v <- split_best["v"]; t <- split_best["t"]
    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]], c(s=-1,v,t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]], c(s=+1,v,t))

    B[,M+1:2] <-cbind(B[,m]*h(x[,v],-1,t),
                      B[,m]*h(x[,v],+1,t))
  } # end for loop over i

  colnames(B) <- paste0("B",(0:(ncol(B)-1))) # optional
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}
bwd_stepwise <- function(fwd,control) {
  #fwd is a list with elements y, B and Bfuncs
  Mmax <- ncol(fwd$B)-1
  Jstar <- 2:(Mmax+1)
  Kstar <- Jstar
  dat <- data.frame(y=fwd$y,fwd$B)
  lofstar <- LOF(y~.-1,dat,control)
  for(M in (Mmax+1):2) {
    b <- Inf
    L <- Kstar
    if(control$trace) cat("L:",L,"\n")
    for(m in L){
      K <- setdiff(L,m)
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~.-1,dat,control)
      if(control$trace) cat("M:K:lof",M,":",K,":",lof,"\n")
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}
LOF <- function(form,data,control) {
  ff <- lm(form,data)
  RSS <- sum(residuals(ff)^2)
  N <- nrow(data)
  M <- length(coef(ff))-1
  Ctilde <- sum(diag(hatvalues(ff))) + control$d*M
  return(RSS * N/(N-Ctilde)^2)
}
h <- function(x,s,t) {
  return(pmax(0,s*(x-t)))
}
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}
#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
new_mars.control <- function(control) {
  structure(control,class="mars.control")
}
validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; setting to 2")
    control$Mmax <- 2
  }
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Setting to ",control$Mmax)
  }
  control
}
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}
