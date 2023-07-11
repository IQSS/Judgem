"reg" <-
function (covars,voteshare,wt=rep(1,length(voteshare))) {
  #cropped missing values here.
  #thold <- cropmiss (covars,voteshare,wt)
  #wt <- as.matrix(thold$w)
  #covars <- unique.columns(as.matrix(thold$covars))
  #voteshare <- as.matrix(thold$voteshare)

  #writeLines(paste(dim(covars)[1],dim(covars)[2]))  #debug

  wtmatinv <- diag(as.numeric(wt)^(-1))

  #debug <<- covars
#  writeLines ("not a matrix debug")
  mom <- t(covars)%*%wtmatinv%*%covars
  beta <- solve(mom)%*%t(covars)%*%wtmatinv%*%voteshare
    
  vshat <- covars%*%beta
  e <- voteshare-vshat
  sig2 <- as.numeric(t(e)%*%e/(dim(covars)[1]-dim(covars)[2]))
  vc <- solve(mom)*sig2

  out <- NULL
  out$beta <- as.matrix(beta)  #beta vector.
  out$vc <- as.matrix(vc)  #variance-covariance matrix.
  out$sig2 <- sig2  #homoskedastic variance parameter.
  out$vshat <- vshat
  return(out)
}

