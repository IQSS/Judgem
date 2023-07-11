"hypit" <-
function (datacaddy,yearnum,delvbar=c(NA,NA),samebg=F,truncit=T) {
  #first, calculate delta.
  delta <- 0
  gut <- datacaddy$fullrow[[yearnum]]
  predval <- (!datacaddy$predict)*datacaddy$lam
  if (is.na(delvbar[2])&!is.na(delvbar[1])) delta <- delvbar[1] else
  if (!is.na(delvbar[2])&is.na(delvbar[1])) {
    mu <- predval*datacaddy$voteshare[[yearnum]] + (datacaddy$covarsnew[[yearnum]]-predval*datacaddy$covars[[yearnum]])%*%datacaddy$beta[[yearnum]]
    delta <- delvbar[2]-weighted.mean(mu,datacaddy$distweights[[yearnum]])
  }
  
  #now: do we have a simulation base? - judgem.sm

  #do it on sight for now.
  sims.t <- simmaker (datacaddy,yearnum)

  safi <- dim(sims.t$c4)
  #debug3 <<- safi
  #debug4 <<- datacaddy$voteshare[[yearnum]]
  rs <- safi[1] 
  tsim <- safi[2]
  simdepth <- safi[3]
  ypart <- array(datacaddy$lam*(!datacaddy$predict)*as.numeric(datacaddy$voteshare[[yearnum]]),c(rs,simdepth))

  i1 <- sample(simdepth); i2 <- sample(simdepth); i3 <- sample(simdepth)

  bgd <- (datacaddy$covarsnew[[yearnum]]-datacaddy$lam*(!datacaddy$predict)*datacaddy$covars[[yearnum]])%*%sims.t$c1[,i1] +
    ypart + delta +
      sims.t$c2[,i2] +
        datacaddy$predict*sims.t$c3[,i3] 

  if (!samebg) { #this works.
    i4 <- sample(simdepth,1) 
    out <- bgd + 1*(!datacaddy$svEvar)*sims.t$c4[,i4,] 
  } else {
    i4 <- sample(simdepth) 
    template <- array(0, safi) 
    for (i in 1:simdepth) template[,,i] <- template[,,i] + bgd[,i]
    out <- template + 1*(!datacaddy$svEvar)*sims.t$c4[,,i4]
  }
  if (truncit) trunc01 (out) else out
}

