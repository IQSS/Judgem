"freq" <-
function (datacaddy,yearnum,vprop,condition=NULL) { 

  #condition <- as.logical(condition)
  if (is.null(condition)) condition <- 1:length(datacaddy$voteshare[[yearnum]])
  sim <- hypit (datacaddy,yearnum,truncit=T)
#  debug <<- sim
#  debug2 <<- condition
#  stop()
  sim2 <- as.numeric(mcmean(inrange(sim,vprop[1],vprop[2])[condition,],datacaddy$seatsper[[yearnum]][condition]))
#  sim2 <- inrange(sim,vprop)*weight$s
#  s3 <- apply(sim2,2,sum)
  #debug <<- sim2
  out <- array(c(mean(sim2),sd(sim2)),c(1,2))
  colnames(out) <- c("Mean Seat Share","SD")
  rownames(out) <- paste(signif(vprop[1],3),"-",signif(vprop[2],3),sep="")
  out
}

