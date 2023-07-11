"setdata" <-
function (covars=NULL,voteshare=NULL,actvotes=NULL,elgvotes=NULL,seatsper=NULL,same.districts=c(0,rep(1,length(covars)-1)),
  use.last.votes=T,uncons.method="impute",uncons.low=0.05,uncons.low.new=0.25,
    uncons.high=0.95,uncons.high.new=0.75,predict=F,simnum=102,simdepth=simnum,prelim=T,weight="constant") {

  flag <- F

  out <- list(covars=covars,voteshare=voteshare,actvotes=actvotes)
  
  if (is.null(actvotes)) {
    writeLines ("Total voters unknown, so let's say 42 per district.")
    actvotes <- new.list(length(covars))
    for (ii in 1:length(covars)) actvotes[[ii]] <- rep(42,dim(covars[[ii]])[1])
    #flag <- T
    out$actvotes <- actvotes
  }

  if (is.null(elgvotes)) {
    writeLines ("The total number of eligible voters was not given. Eligible voters set to actual voters.")
    out$elgvotes <- actvotes
    elgvotes <- actvotes
  } else out$elgvotes <- elgvotes

 # writeLines("remv check 1")
  
  for (i in 1:length(covars)) {
    if (is.null(covars[[i]])|all(is.na(covars[[i]]))) {
      writeLines (paste("No valid covariates given for year ",i,". Substituting constant row only.",sep=""))
      covars[[i]] <- as.matrix(rep(1,length(voteshare[[i]])))
    }
#    writeLines(paste(dim(covars[[i]])[1],dim(covars[[i]])[2]))
    nas <- NULL
    for (k in 1:dim(covars[[i]])[2]) {
      #writeLines(paste(k))
      if (all(is.na(covars[[i]][,k]))) nas <- c(nas,k)
    }
    if (!is.null(nas)) covars[[i]] <- as.matrix(covars[[i]][,-nas])
    covars[[i]] <- o(covars[[i]])  #adds constant row. could be a debug point.
#####
#    writeLines(paste(dim(covars[[i]])[1],"by",dim(covars[[i]])[2]))
    out$covars <- covars
  }

 
  
  for (i in 1:length(covars)) if (any( dim(covars[[i]]) != dim(unique.columns(covars[[i]])) ) ) stop(paste("Some columns of covariates in year",i,"are equal. Make each column unique before proceeding."))
    
  if (is.null(voteshare)) stop ("Can't do an analysis without results.")
  

  #now, need to incorporate NA values - ignore when needed?
  for (i in 1:length(voteshare)) {
    ct <- voteshare[[i]]
    ct <- ct[!is.na(ct)]
    if (any(trunc01(ct)!=ct)) stop (paste("Some districts have vote proportions outside the [0,1] range, in particular year",i,"."))
  }

  if (is.null(seatsper)) {
    writeLines ("The total number of seats per district was not given, and is so assumed to be 1 in all elections.")
    ones <- function (voteshare) array(as.numeric(voteshare[[i]]>=0),dim(voteshare))
    seatsper <- lapply (voteshare,ones)
    out$seatsper <- seatsper
  } else out$seatsper <- seatsper

#  writeLines("remv check 3")
  

  possible.uncs <- c("impute","default","remove","nochange")
  if (!any(uncons.method==possible.uncs)) {
    writeLines ("Your choice for removing uncontesteds does not match an existing option. Assuming no change.")
    uncons.method <- "nochange"
  }
  
  #Now all five variables are in place. Put them in matrix form. Missing values stay for now.
  out$fullrow <- new.list(length(covars))
  for (i in 1:length(out$voteshare)) {
    caddy <- cbind (out$voteshare[[i]],out$covars[[i]],out$elgvotes[[i]],out$actvotes[[i]],out$seatsper[[i]])
    out$fullrow[[i]] <- missed(caddy)

    out$voteshare[[i]] <- as.matrix(out$voteshare[[i]])
    out$covars[[i]] <- as.matrix(out$covars[[i]])
    out$elgvotes[[i]] <- as.matrix(out$elgvotes[[i]])
    out$actvotes[[i]] <- as.matrix(out$actvotes[[i]])
    out$seatsper[[i]] <- as.matrix(out$seatsper[[i]])

  }


  out$uncL <- uncons.low    #maximum for Repub. uncontested
  out$uncLR <- uncons.low.new   #new level of Rep. uncontested
  out$uncU <- uncons.high    #min. for Dem uncontested
  out$uncUR <- uncons.high.new   #new level of Dem uncontested

  out$predict <- predict   #predict, rather than counterfact? yes.
  out$svEvar <- F    #Do we want the expected value (1) or the total variation (0)?
  out$sims <- simdepth  #default number of simulations to run within one block.
  out$simd <- simnum  #Number of blocks.
  
  same.districts[1] <- F #can't go before the first.
  #year if we're in the last one
  out$same.districts <- same.districts

  out$covarsnew <- out$votesharenew <- new.list(length(covars))
  
#initialize the possibility for simulations.
#  out$id <- NULL

  #now, diagnose the state of the system.
  if ((!flag)&(length(covars)!=length(voteshare))) {
    writeLines("Error: Covariates and votes don't have the same number of years.")
    out <- NULL
  } else
  if ((length(voteshare)!=length(actvotes))|(length(voteshare)!=length(seatsper))|(length(voteshare)!=length(elgvotes))) {
    writeLines("Error: Votes and weights don't have the same number of years.")
    out <- NULL
  } else for (i in 1:length(covars)) {
      if (dim(covars[[i]])[1]!=length(voteshare[[i]])) {
        writeLines (paste("Error: Covariates and votes in data set",i,"do not have the same number of districts."))
        out <- NULL
      }  
      if ((length(voteshare[[i]])!=length(actvotes[[i]]))|(length(voteshare[[i]])!=length(seatsper[[i]]))|(length(voteshare[[i]])!=length(elgvotes[[i]]))) {
        writeLines (paste("Error: Votes and weights in data set",i,"do not have the same number of districts."))
        out <- NULL
	}
    } 
 
#OK. If all is well, out isn't NULL. So now we look at imputation, replacement or whatever.
#options: impute, remove, default, nochange

  if (flag) {
    out <- NULL
    writeLines ("There was an error loading the data.")
  } else {
    class(out) <- "judgem"
    writeLines ("Data loading worked.")
  }
  
  if (is.null(out)) stop() else {
    #debug <<- out
    out <- fix.uncons (out,same.districts,uncons.method)
    #debug3 <<- out
    if (use.last.votes) for (ii in 2:length(out$covars)) 
      if (same.districts[[ii]]) {out$covars[[ii]] <- unique.columns(cbind(out$covars[[ii]],out$voteshare[[ii-1]])); colnames(out$covars[[ii]])[dim(out$covars[[ii]])[2]] <- "lastvote"}
    #debug2 <<- out
    if (prelim) {
        writeLines ("Conducting preliminary analysis.") 
	out <- prelim(out,weight)
    }
    return(out)
  }

}

