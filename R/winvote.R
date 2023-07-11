"winvote" <-
function (datacaddy,yearnum,pr) {
  work <- function(vs) {
   lo <- 0; hi <- 1;probwin <- 9999;res <- 0.5;ii <- 0;bogus <- F
   patho <- NULL

   if ((pr>=0)&(pr<=1)) while (abs(probwin-pr)>0.005) {
    res <- mean(c(lo,hi))
    vsim <- vs-0.5+res
    probwin <- mean(mcmean(v2s(rb(vsim,datacaddy$extras[,1])),c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3])))

#    writeLines(paste(pr,probwin,pr>probwin))
    #making it blurrier.

    if (pr>probwin) lo <- mean(c(lo,res)) else hi <- mean(c(hi,res))
    patho <- rbind(patho,c(pr,probwin,lo,hi))

#    c(pr,probwin,lo,hi)
    
    ii <- ii+1
    if (ii == 100) {
      writeLines ("Too many repetitions...")
      probwin <- pr
      bogus <- T
    }
#    writeLines(paste(ii))
    if (ii>10) if (all(patho[ii,2]==patho[(ii-9):(ii-1),2])) {
      res <- -probwin
      probwin <- pr
    }
   } else {writeLines ("Proportion isn't between 0 and 1."); bogus <- T}
   if (!bogus) res else {errcheck <<- patho; writeLines(paste(ii)); NA}
  }

  vs0 <- hypit(datacaddy,yearnum,c(NA,0.5),F,truncit=F)
  mstat <- work(vs0)
  vs1 <- hypit(datacaddy,yearnum,c(NA,0.5),T,truncit=F)
  queue <- apply(vs1,3,work)
  sdstat <- sd(queue[queue>=0])

  ms <- sum(1*(queue<0))
  if (ms>0) writeLines (paste(ms,"of",length(queue),"converged erroneously."))
  
  if (ms>0) out <- list(mstat,sdstat,queue) else {out <- array(c(mstat,sdstat),c(1,2)); colnames(out) <- c("Vote Share","SD"); rownames(out) <- paste("P(win) =",pr)}

  return(out)                                                    
}

