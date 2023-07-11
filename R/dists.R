"dists" <-
function (datacaddy,yearnum,plotornot) {  #this method works for all of the above
  lamb <- datacaddy$lam*1*(!datacaddy$predict)
  qun <- datacaddy$covarsnew[[yearnum]]-lamb*datacaddy$covars[[yearnum]]
  #debug <<- qun
  #debug2 <<- lamb
  mu <- lamb*datacaddy$voteshare[[yearnum]] + qun%*%datacaddy$beta[[yearnum]]
  std <- sqrt(diag((1-lamb^2)*datacaddy$sig^2*(!datacaddy$svEvar) + qun%*%datacaddy$vc[[yearnum]]%*%t(qun)))
  pwin <- 1-pnorm(0.5,mu,std)
  lnvprb <- tiep(mu,std,datacaddy$actvotes[[yearnum]]) #should be turnout.
  out <- cbind(mu,std,pwin)
  if (datacaddy$svEvar) item <- "P(E(vote)>0.5)" else item <- "P(vote>0.5)"
  if (all(datacaddy$covarsnew[[yearnum]]==datacaddy$covars[[yearnum]])) item2 <- "Expected Vote" else
    item2 <- "Predicted Vote"
  colnames (out) <- c(item2,"Std. Dev.",item)
  out <- cbind(datacaddy$voteshare[[yearnum]],out); colnames(out)[1] <- "Observed Vote"
  cn <- colnames(out)
  out <- cbind (out,exp(lnvprb),lnvprb)
  colnames (out) <- c(cn,"P(decisive)","ln(P(decisive))")
  #if (plotornot) {if (datacaddy$predict) kernel.plot (mu,xlab="Vote Share",ylab="Likelihood of District Vote",main="District Vote Likelihood Plot") else {kernel.plot(mu,as.numeric(datacaddy$voteshare[[yearnum]]),xlab="Vote Share",ylab="Likelihood of District Vote",main="District Vote Likelihood Plot"); text(0.9,1.3,"Observed Votes",col=2)}; text(0.9,1.4,"Model Prediction",col=1); }

  return(out)
}

