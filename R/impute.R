"impute" <-
function (covars,voteshare,present) {
  #get regression.
  #debug <<- cbind(covars,voteshare)
  rstf <- reg(covars[present,],voteshare[present])
  #now, get values.
 
  pv <- rnorm(length(rstf$vshat),rstf$vshat,sqrt(rstf$sig2))
  yt <- rep(NA,length(voteshare)); yt[present] <- pv
  pv <- trunc01(pv) 

  out <- voteshare
  for (i in 1:length(voteshare)) if (is.na(voteshare[i])) out[i] <- pv[i]
  return(out)
}

