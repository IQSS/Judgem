"winprob" <-
function (datacaddy,yearnum,lohi,delvbar=c(NA,NA)) {

  if (is.null(lohi)) lohi <- c(0,1)

  test3 <- function() apply(v2s(rbx(vsim2,datacaddy$extras[,1])),3,apply,2,weighted.mean,c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3]))
  irsd <- function() sd(apply(inrange(test3(),lohi[1],lohi[2]),2,mean))

  if (lohi[1]<=0) lohi[1] <- -1e6; if (lohi[2]>=1) lohi[2] <- 1e6
  #different gammas.
  vsim <- hypit(datacaddy,yearnum,delvbar,F)
  mstat <- mean(inrange(apply
                        (trunc01(rb(vsim,datacaddy$extras[,1])),2,weighted.mean,c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3])),lohi[1],lohi[2]))
  
  vsim2 <- hypit (datacaddy,yearnum,delvbar,T)
  sdstat <- irsd()

  out <- matrix(c(mstat,sdstat),1)
  colnames(out) <- c("Mean","SD of Mean")
  out
}

