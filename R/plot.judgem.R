"plot.judgem" <-
function(x,straight.up=F,filename=NULL,year=1) {
  judgem.object <- x
  if (is.null(filename)) x11() else png(paste(filename,".png",sep=""))

  if (length(judgem.object$outputyear)>0) yrfull <- judgem.object$years[judgem.object$outputyear] else
    yrfull <- year

  switch (judgem.object$outputclass,
          #"none" = {writeLines (paste("Plotting year,",year))
          #          yy <- judgem.object$voteshare[[year]]
          #          yy <- yy[!is.na(yy)]
          #          kernel.plot(yy)},
          "seats" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Seats-Votes Plot for",yrfull),
                 xlab="Vote Proportion",ylab="Seat Proportion")
            yearnum <- judgem.object$outputyear
            t.x <- as.numeric(rownames(judgem.object$output))
            t.y <- judgem.object$output[,1]
            t.s <- judgem.object$output[,2]
            lines(t.x,t.y)
            lines(t.x,t.y+2*t.s,col=3)
            lines(t.x,t.y-2*t.s,col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
            xx <- weighted.mean(judgem.object$voteshare[[yearnum]],judgem.object$distweights[[yearnum]],na.rm=T)
            yy <- weighted.mean(v2s(judgem.object$voteshare[[yearnum]]),judgem.object$seatsper[[yearnum]],na.rm=T)
            points(xx,yy,col=4)
            text(xx,yy,paste("Vote:",signif(xx,4)),pos=4)
            text(xx,yy-0.05,paste("Seats:",signif(yy,4)),pos=4)
          },
          "prob" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Winning Probability-Votes Plot for",yrfull),xlab="Vote Proportion",ylab="Party 1's Win Probability")
            t.x <- as.numeric(rownames(judgem.object$output))
            t.y <- judgem.object$output[,1]
            t.s <- judgem.object$output[,2]
            lines(t.x,t.y)
            lines(t.x,t.y+2*t.s,col=3)
            lines(t.x,t.y-2*t.s,col=3)
          },
          "distreport" = {
            kernel.plot (judgem.object$output[,1],judgem.object$output[,2],
              xlab="Vote Share",ylab="Likelihood of District Vote",main=paste("District Vote Likelihood Plot for",yrfull))
            text(0.9,1.3,"Observed Votes",col=1)
            text(0.9,1.4,"Model Prediction",col=2);
          },
          {year <- judgem.object$outputyear
           writeLines (paste("Outputting a kernel plot of districts in ",yrfull,"."))
           yy <- judgem.object$voteshare[[year]]; yy <- yy[!is.na(yy)]; kernel.plot(yy)})
  if (!is.null(filename)) dev.off()
}

