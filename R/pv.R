"pv" <-
function (datacaddy,yearnum,Ev=NULL,delta=NULL,lohi=NULL,
 condition=rep(1,length(datacaddy$voteshare[[yearnum]]))) {

  if (is.null(Ev)&is.null(delta)) stop ("Error in prob-votes: no proportions or shifts specified.")
  if (is.null(lohi)) lohi <- c(0.5,1e6)
  condition <- as.logical(condition)
 
  irmean <- function (deltafac) mean(inrange(
    apply (trunc01(rb(vsim+deltafac,datacaddy$extras[,1])),2,weighted.mean,c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3])),
    lohi[1],lohi[2]))
  test3 <- function(deltafac) apply( v2s(rbx(vsim2+deltafac,datacaddy$extras[,1])) ,3, apply,2,weighted.mean, c(datacaddy$seatsper[[yearnum]],datacaddy$extras[,3]))
  irsd <- function(deltafac) sd(apply(inrange(test3(deltafac),lohi[1],lohi[2]),2,mean))
  
  delvbar <- c(NA,NA) #no adjustments here.

  if (is.null(delta)) {
    predval <- (!datacaddy$predict)*datacaddy$lam
    mu <- predval*datacaddy$voteshare[[yearnum]] + (datacaddy$covarsnew[[yearnum]]-predval*datacaddy$covars[[yearnum]])%*%datacaddy$beta[[yearnum]]
    delta <- Ev-weighted.mean(mu,datacaddy$actvotes[[yearnum]])
  }
  
  lohi <- c(0.5,1e6) #prob. democratic win
  datacaddy$seatsper[[yearnum]] <- datacaddy$seatsper[[yearnum]][condition]
  vsim <- hypit (datacaddy,yearnum,delvbar,F,truncit=F)[condition,]
  stat <- sapply(delta,irmean)
 
  vsim2 <- hypit (datacaddy,yearnum,delvbar,T,truncit=F)[condition,,] 
  sdstat <- sapply(delta,irsd)
 
  out <- cbind (stat,sdstat)
  colnames(out) <- c("Pr(win) Mean","SD")
  rownames(out) <- Ev
  out
}
