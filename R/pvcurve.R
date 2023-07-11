"pvcurve" <-
function (datacaddy,yearnum,num=NULL,lohi=NULL) {

  if (is.null(lohi)) lohi <- c(mean(datacaddy$voteshare[[yearnum]])-0.15,mean(datacaddy$voteshare[[yearnum]])+0.15)
  if (is.null(num)) num <- 10
  Ev <- lohi[1]+(0:num)*abs(lohi[2]-lohi[1])/num
  valpred <- pv(datacaddy,yearnum,Ev)
  plot(c(0,1),c(0,1),ty="n",main="Winning Probability-Votes Plot",xlab="Vote Proportion",ylab="Democratic Win Probability")
  lines(Ev,valpred[,1])
  lines(Ev,pmin(valpred[,1]+2*valpred[,2],rep(1,dim(valpred)[1])),col=3)
  lines(Ev,pmax(valpred[,1]-2*valpred[,2],rep(0,dim(valpred)[1])),col=3)

  lines(c(0.5,0.5),c(0,1),col=8)
  lines(c(0,1),c(0.5,0.5),col=8)

  return(valpred)
}

