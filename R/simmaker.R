"simmaker" <-
function (datacaddy,which=1:length(data$voteshare)) {
 #if (!exists("judgem.sm")) judgem.sm <<- list(NULL)
 for (ii in which) {
  rs <- dim(datacaddy$covars[[ii]])[1] #rows.
  sig2 <- diag(datacaddy$sig^2,rs) #later, this might really be mvnorm with spatial considerations.)

  bsim <- t(rmvnorm(datacaddy$simd,datacaddy$beta[[ii]],datacaddy$vc[[ii]]))
  q1 <- t(rmvnorm(datacaddy$simd,rep(0,rs),datacaddy$lambda*(1-datacaddy$la)*sig2))
  q2 <- t(rmvnorm(datacaddy$simd,rep(0,rs),datacaddy$la^2*sig2))

  r1 <-array(rnorm(rep(0,rs*datacaddy$sims*datacaddy$simd),0,sqrt(1-datacaddy$la)*datacaddy$sig),c(rs,datacaddy$sims,datacaddy$simd))
  
  out <- NULL
#these are rs-by-simd.
  out$c1 <- bsim  #only if counterfactual.
  out$c2 <- q1  #if counterfactual, need only q1.
  out$c3 <- q2  #if predictive, need q1 and q2.

#this is rs-by-simd-by-basesims
  out$c4 <- r1  #if total variation, not variation in expected value.
  #judgem.sm[[datacaddy$id[i]]] <<- out
 }
 writeLines ("Ran simulations.")
#now, spit it out.
 out
}

