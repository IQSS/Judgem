"voting.power" <-
function (datacaddy,yearnum,vot,tot) {

  #a couple of checks.
  flag <- F
  if (any(dim(vot)!=dim(tot))) {
    writeLines ("Dimensions of voters and eligible voters don't match.")
    flag <- T
  }
  if (dim(vot)[1]!=length(datacaddy$voteshare[[yearnum]])) {
    writeLines ("Dimensions of voters and eligible voters don't match.")
    flag <- T
  }

  if (!flag) {
  datacaddy$predict <- F
  lamb <- datacaddy$lam
  qun <- datacaddy$covarsnew[[yearnum]]-lamb*datacaddy$covars[[yearnum]]
  mu <- lamb*datacaddy$voteshare[[yearnum]] + qun%*%datacaddy$beta[[yearnum]]
  std <- sqrt(diag((1-lamb^2)*datacaddy$sig^2*(!datacaddy$svEvar) + qun%*%datacaddy$vc[[yearnum]]%*%t(qun)))
  pwin <- 1-pnorm(0.5,mu,std)

  find.p.all <-  function (m,s,wt) {
    lvp <- tiep(m,s,wt)
    mp <-  weighted.mean(lvp,wt)
    sdp <- sqrt(sum((lvp-mp)^2)/(length(m)-1))
    cbind(exp(lvp),lvp,sdp)
  }

  out <-
  cbind(find.p.all(mu,std,datacaddy$actvotes[[yearnum]]),find.p.all(mu,std,datacaddy$elgvotes[[yearnum]]-datacaddy$actvotes[[yearnum]]),
        find.p.all(mu,std,datacaddy$elgvotes[[yearnum]]))

  colnames(out) <- c("P(decisive voter)","ln(P(decisive))","SD(ln(P(decisive)))",
                   "P(decisive non-voter)","ln(P(decisive))","SD(ln(P(decisive)))",
                          "P(decisive both)","ln(P(decisive))","SD(ln(P(decisive)))")
  out2 <- NULL; out2$fullres <- out

  #decisiveness of voters.
  eieio <- NULL

  p <- function(a,b) a*b
  mn <- function (a,b) a-b

  ei.worker <- function(eiwt,num) {
    vdmat <- apply (eiwt,2,p,out[,num])

    #Adding the contribution of multiple seats. Could be wrong, but hey. 9-5-05
    vdmat <- apply (vdmat,2,p,datacaddy$seatsper[[yearnum]])
    
    sdvdm <- t(apply (vdmat,1,mn,apply(vdmat,2,mean)))
    cbind(apply(vdmat,2,mean),sqrt(apply(sdvdm^2,2,mean)/(dim(vdmat)[1]-1)))
  }

  eieio <- cbind(ei.worker(vot,1),ei.worker(tot-vot,4),ei.worker(tot,7))
  eieio <-  eieio/max(eieio[,1]) #compare all values relative to 1.
  colnames (eieio) <- c("Voter Power","SD","Non-Voter Power","SD","Member power","SD")
  
  out2$ei <- eieio
  return(out2)} else return(NULL)
}

