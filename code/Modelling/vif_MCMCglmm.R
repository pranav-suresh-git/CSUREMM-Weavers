vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
  nF <- fit$Fixed$nfl
  v <- cov(as.matrix(fit$X[,1:nF]))
  nam <- colnames(fit$Sol[,1:nF])
  
  v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
  nam <- nam[-intercept.columns]
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
