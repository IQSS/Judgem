"judgem.setdata" <-
judgem.setdata <- function (covars=NULL,voteshare=NULL,actvotes=NULL,elgvotes=NULL,seatsper=NULL,
  same.districts=NULL,
    use.last.votes=T,uncons.method="impute",uncons.low=0.05,uncons.low.new=0.10,
      uncons.high=0.95,uncons.high.new=0.90,simnum=102,simdepth=simnum,prelim=T,weight="constant") {

  flag <- F

  out <- list(covars=covars,voteshare=voteshare,actvotes=actvotes)

  if (is.null(same.districts)) {
    quant <- 0
    if (length(voteshare)>1) for (ii in 2:length(voteshare)) quant <- c(quant,1*(length(voteshare[[ii]])==length(voteshare[[ii-1]])))
    same.districts <- quant
  }
  
  if (is.null(actvotes)) {
    writeLines ("Number of actual voters unknown, so let's say 42 per district.")
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

  #writeLines("debug check 1")
  
  for (ii in 1:length(covars)) {
    if (is.null(covars[[ii]])|all(is.na(covars[[ii]]))) {
      writeLines (paste("No valid covariates given for year ",ii,". Substituting constant row only.",sep=""))
      covars[[ii]] <- as.matrix(rep(1,length(voteshare[[ii]])))
    }
#    writeLines(paste(dim(covars[[i]])[1],dim(covars[[i]])[2]))
    nas <- NULL
    for (k in 1:dim(covars[[ii]])[2]) {
      #writeLines(paste(k))
      if (all(is.na(covars[[ii]][,k]))) nas <- c(nas,k)
    }
    if (!is.null(nas)) covars[[ii]] <- as.matrix(covars[[ii]][,-nas])
    covars[[ii]] <- o(covars[[ii]])  #adds constant row. could be a debug point.
#####
#    writeLines(paste(dim(covars[[i]])[1],"by",dim(covars[[i]])[2]))
    out$covars <- covars
  }

  #writeLines("debug check 2")
  
  #for (ii in 1:length(covars)) if (any( dim(covars[[ii]]) != dim(unique.columns(covars[[ii]])) ) ) warning(paste("Some columns of covariates in year",ii,"are equal. Judgem has cropped these columns."))
    
  if (is.null(voteshare)) stop ("Can't do an analysis without results.")
  

  #now, need to incorporate NA values - ignore when needed?
  for (ii in 1:length(voteshare)) {
    ct <- voteshare[[ii]]
    ct <- ct[!is.na(ct)]
    if (any(trunc01(ct)!=ct)) stop (paste("Some districts have vote proportions outside the [0,1] range, in particular year",ii,"."))
  }

  if (is.null(seatsper)) {
    writeLines ("The total number of seats per district was not given, and is therefore assumed to be 1 in all elections.")
    onus <- function (vs) 1*(vs>=0)
    seatsper <- lapply (voteshare,onus)
    tstt <<- voteshare
    out$seatsper <- seatsper
  } else out$seatsper <- seatsper

  tsts <<- seatsper
  #writeLines("debug check 3")
  

  possible.uncs <- c("impute","default","remove","nochange")
  if (!any(uncons.method==possible.uncs)) {
    writeLines ("Your choice for removing uncontesteds does not match an existing option. Assuming no change.")
    uncons.method <- "nochange"
  }
  
  #Now all five variables are in place. Put them in matrix form. Missing values stay for now.
  out$fullrow <- new.list(length(out$covars))
  for (ii in 1:length(out$voteshare)) {
    caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$elgvotes[[ii]],out$actvotes[[ii]],out$seatsper[[ii]])
    out$fullrow[[ii]] <- as.numeric(missed(caddy))

    out$voteshare[[ii]] <- as.matrix(out$voteshare[[ii]])
    out$covars[[ii]] <- as.matrix(out$covars[[ii]])
    out$elgvotes[[ii]] <- as.matrix(out$elgvotes[[ii]])
    out$actvotes[[ii]] <- as.matrix(out$actvotes[[ii]])
    out$seatsper[[ii]] <- as.matrix(out$seatsper[[ii]])
  }

  
  out$uncL <- uncons.low    #maximum for Repub. uncontested
  out$uncLR <- uncons.low.new   #new level of Rep. uncontested
  out$uncU <- uncons.high    #min. for Dem uncontested
  out$uncUR <- uncons.high.new   #new level of Dem uncontested

  out$svEvar <- F    #Do we want the expected value (1) or the total variation (0)?
  out$sims <- simdepth  #default number of simulations to run within one block.
  out$simd <- simnum  #Number of blocks.
  
  out$covarsnew <- new.list(length(covars))
  out$same.dists <- same.districts
  out$outputclass <- "none"
  out$outputyear <- NULL
  
  #debug2 <<- out
  
  #now, diagnose the state of the system.
  if ((!flag)&(length(covars)!=length(voteshare))) {
    stop("Error: Covariates and votes don't have the same number of years.")
    out <- NULL
  } else
  if ((length(voteshare)!=length(actvotes))|(length(voteshare)!=length(seatsper))|(length(voteshare)!=length(elgvotes))) {
    stop("Error: Votes and weights don't have the same number of years.")
    out <- NULL
  } else for (ii in 1:length(covars)) {
      if (dim(covars[[ii]])[1]!=length(voteshare[[ii]])) {
        stop(paste("Error: Covariates and votes in data set",ii,"do not have the same number of districts."))
        out <- NULL
      }  
      if ((length(voteshare[[ii]])!=length(actvotes[[ii]]))|(length(voteshare[[ii]])!=length(seatsper[[ii]]))|(length(voteshare[[ii]])!=length(elgvotes[[ii]]))) {
        stop(paste("Error: Votes and weights in data set",ii,"do not have the same number of districts."))
        out <- NULL
	}
    } 
 
#OK. If all is well, out isn't NULL. So now we look at imputation, replacement or whatever.
#options: impute, remove, default, nochange

  if (flag) {
    out <- NULL
    stop("There was an error loading the data.")
  } else {
    class(out) <- "judgem"
    writeLines ("Data loading worked.")
  }

  test <<- out
  
  if (is.null(out)) stop() else {
    #writeLines ("debug stuff")
    out <- fix.uncons (out,same.districts,uncons.method)
    #writeLines ("debug stuff")
    if ((use.last.votes)&(length(out$covars)>1)) for (ii in 2:length(out$covars)) 
      if (same.districts[[ii]]) {out$covars[[ii]] <- unique.columns(cbind(out$covars[[ii]],out$voteshare[[ii-1]])); colnames(out$covars[[ii]])[dim(out$covars[[ii]])[2]] <- "lastvote"}
    #writeLines ("debug stuff")

    for (ii in 1:length(out$voteshare)) {
      caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$elgvotes[[ii]],out$actvotes[[ii]],out$seatsper[[ii]])
      out$fullrow[[ii]] <- as.numeric(missed(caddy))
      out$covars[[ii]] <- unique.columns(out$covars[[ii]],keepers=out$fullrow[[ii]])
    }

    if (prelim) {
        writeLines ("Conducting preliminary analysis.") 
	out <- prelim(out,weight)
    }
    return(out)
  }

}

