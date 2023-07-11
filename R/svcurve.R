"svcurve" <-
function (datacaddy,yearnum,num=100,lohi=c(mean(datacaddy$voteshare[[yearnum]])-0.15,mean(datacaddy$voteshare[[yearnum]])+0.15)) {
  if (is.null(num)) num <- 100
  if (is.null(lohi)) lohi <- c(mean(datacaddy$voteshare[[yearnum]])-0.15,mean(datacaddy$voteshare[[yearnum]])+0.15)

  pts <- lohi[1]+0:num*abs(lohi[2]-lohi[1])/num
  valpred <- sv(datacaddy,yearnum,pts)
  plot(c(0,1),c(0,1),ty="n",main="Seats-Votes Plot",xlab="Vote Proportion",ylab="Seat Proportion")
  lines(pts,valpred$Es)
  lines(pts,valpred$Es+2*valpred$SDsv,col=3)
  lines(pts,valpred$Es-2*valpred$SDsv,col=3)
  lines(c(0.5,0.5),c(0,1),col=8)
  lines(c(0,1),c(0.5,0.5),col=8)
  xx <- weighted.mean(datacaddy$voteshare[[yearnum]],datacaddy$distweights[[yearnum]])
  yy <- weighted.mean(v2s(datacaddy$voteshare[[yearnum]]),datacaddy$seatsper[[yearnum]])
  points(xx,yy,col=4)
  text(xx,yy,paste("Vote: ",signif(xx,4),", Seats: ",signif(yy,4),sep=""),pos=4)
  return(valpred)
}

