"svsum" <-
function (datacaddy,yearnum) {
  datacaddy$svEvar <- F; #samebg<-T
  seatsper <- c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3])
  delvbar <- c(NA,0.5)
#quantities desired in each district: bias and responsiveness for range, center

  intvl <- 45:55/100
  out <- matrix(rep(0,4),1)
  vsim <- hypit (datacaddy,yearnum,delvbar,T)

  #writeLines ("Check 1") #check
  
  sbar <- array(0,c(dim(vsim)[2],length(intvl),dim(vsim)[3]))
  for (i in 1:length(intvl)) {
    tt <- v2s(rbx(vsim-0.5+intvl[i],datacaddy$extras[,1]))
    sbar[,i,] <- mcmean3d(tt,seatsper)
  }

  #writeLines ("Check 2") #check
  #distinct areas: 1-5,6,7-11
  rlow <- 1:5
  rmid <- 6
  rhigh <- 7:11; rhight <- c(11,10,9,8,7)  
  #midpoint bias:
  o1 <- apply((2*sbar[,rmid,]-1)/2,2,mean)

  #spread bias:
  o2 <- apply((sbar[,rlow,]-(1-sbar[,rhight,]))/2,3,mean)

  #responsiveness at 0.45, 0.55
  o3 <- apply((sbar[,11,]-sbar[,1,])/(0.55-0.45),2,mean)

  #responsiveness at mean vote value
  mv <- weighted.mean(datacaddy$voteshare[[yearnum]],datacaddy$actvotes[[yearnum]]); intvl <- c(mv-0.01,mv+0.01)
  sbar <- array (0,c(dim(vsim)[2],length(intvl),dim(vsim)[3]))

  #writeLines ("Check 3")  #check
  for (i in 1:length(intvl)) {
    tt <- rbx(vsim-0.5+intvl[i],datacaddy$extras[,1])
    for (j in 1:dim(tt)[2]) sbar[,i,j] <- mcmean(v2s(tt[,,j]),c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3]))
  }

  o4 <- apply((sbar[,2,]-sbar[,1,])/(intvl[2]-intvl[1]),2,mean)
  out <- cbind (o1,o2,o3,o4)
  
  mres <- apply(out,2,mean)
  sdres <- apply(out,2,sd)

  out <- cbind(mres,sdres)
  colnames(out) <- c("Mean","SD")
  rownames(out) <- c("Partisan Bias (0.5)","Partisan Bias (0.45-0.55)",
                    "Responsiveness (0.45-0.55)","Responsiveness (observed)")
  signif(out,4)
}

