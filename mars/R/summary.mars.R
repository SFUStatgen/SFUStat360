# Method: summary
summary.mars <- function(object) {
  xn <- object$x_names
  bn <- names(object$coefficients)
  ss <- object$Bfuncs
  cat("Basis functions:\nB0:\n  Intercept\n")
  for(i in 2:length(ss)) {
    cat(paste0(bn[i],":\n"))
    for(j in 2:nrow(ss[[i]])) {
      cat(paste0("  Component ",j-1,":  variable ",xn[ss[[i]][j,"v"]],";"))
      cat(paste0(" sign ",ss[[i]][j,"s"],";"))
      cat(paste0(" split at value ",ss[[i]][j,"t"],"\n"))
    }
  }
  cat("\n\nCoefficients:\n")
  print(object$coefficients)
}
