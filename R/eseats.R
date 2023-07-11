"eseats" <-
function (datacaddy,yearnum,Ev) {
  sig2 <- diag(datacaddy$sig^2*as.numeric(datacaddy$actvotes[[yearnum]])^(0))   #changed / to * for debug, 3-30-06. Back again.
  mult <- datacaddy$lam#*(as.numeric(!datacaddy$predict))
  xpart <- datacaddy$covarsnew[[yearnum]]-mult*datacaddy$covars[[yearnum]]
  mu <- as.numeric(mult*datacaddy$voteshare[[yearnum]]+xpart%*%datacaddy$be[[yearnum]])
#convinced this is better than the old.
  var <- (1-mult^2)*sig2+xpart%*%datacaddy$vc[[yearnum]]%*%t(xpart)
  vars <- diag(var)

  out <- NULL
  out$delta <- Ev-weighted.mean(mu,as.numeric(datacaddy$distweights[[yearnum]]))

  rcmat <- crossadd (mu,out$delta)
  #debug4 <<- rcmat #up to here.
  #debug <<- mu
  #debug2 <<- as.numeric(datacaddy$actvotes[[yearnum]])
  #debug3 <<- out$delta
  #debug4 <<- cbind(datacaddy$covarsnew[[yearnum]],datacaddy$covars[[yearnum]]) #mult This is fine.

  #replace.?
  findp <- function (colt) weighted.mean ( c(1-pnorm(0.5,colt,sqrt(vars)),v2s(datacaddy$extras[,1])), c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3]))
  out$Es <- apply (rcmat,2,findp)

  return(out)
}

