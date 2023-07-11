"dists.eval" <-
function (datacaddy,yearnum,plotornot) { #for evaluation of next year's results under the model.
  datacaddy$predict <- F
  lamb <- datacaddy$lam*as.numeric(!datacaddy$predict)
  qun <- datacaddy$covars[[yearnum+1]]-lamb*datacaddy$covars[[yearnum]]
  mu <- lamb*datacaddy$voteshare[[yearnum]] + qun%*%datacaddy$beta[[yearnum]]
  std <- sqrt(diag((1-lamb^2)*datacaddy$sig^2*((datacaddy$svEvar+1)%%2) + qun%*%datacaddy$vc[[yearnum]]%*%t(qun)))
  pwin <- 1-pnorm(0.5,mu,std)
  lnvprb <- tiep(mu,std,datacaddy$actvotes[[yearnum]]) #should be turnout.

  out <- cbind(mu,std,pwin)
  if (datacaddy$svEvar==1) item <- "P(E(vote)>0.5)" else item <- "P(vote>0.5)"
  colnames (out) <- c("Expected Vote","Std. Dev.",item)
  out <- cbind(datacaddy$voteshare[[yearnum+1]],out); colnames(out)[1] <- "Next Vote"

  cn <- colnames(out)
  out <- cbind (out,exp(lnvprb),lnvprb)
  colnames (out) <- c(cn,"P(decisive)","ln(P(decisive))")

  if (plotornot) {
    x11()
    plot (c(0,1),c(0,1),main="Comparing Actual Votes to Predicted
Mean Votes", ylab="Predicted Votes",xlab="Actual Votes",ty="l")
    points (datacaddy$voteshare[[yearnum+1]],mu,col=2)
    #lines(c(0,1),c(0,1)
    for (i in 1:length(mu)) lines (rep(datacaddy$voteshare[[yearnum+1]][i],2),c(mu[i]+2*std[i],mu[i]-2*std[i]),col=3)
  }  

  return(out)
}

