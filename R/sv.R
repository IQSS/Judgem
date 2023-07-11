"sv" <-
function (datacaddy,yearnum,Ev) {
  eres <- eseats (datacaddy,yearnum,Ev)
  vsim0 <- hypit(datacaddy,yearnum,truncit=F)
  
  SDsv <- Ev
  for (i in 1:length(Ev)) {
    vsim <- vsim0+eres$delta[i]

    vsim <- rb(vsim,datacaddy$extras[,1]) #a difference here. 9-6-05
    
    vbar <- as.numeric(mcmean(trunc01(vsim),c(datacaddy$actvotes[[yearnum]],datacaddy$extras[,4])))
    sbar <- as.numeric(mcmean(v2s(vsim),c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3]) ) )
    SDsv[i] <- sqrt(var(sbar)-(cov(vbar,sbar)^2/var(vbar)))
  }
  out <- NULL
  out$Es <- eres$Es
  out$SDsv <- SDsv
  
  out <- as.data.frame(out); rownames(out) <- Ev; colnames(out) <- c("Seats Mean","SD"); out
}
