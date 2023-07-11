"judgem" <-
function (modelform=~1,voteform=NULL,same.districts=NULL,data,
                    uncons.method="default",uncons.low=0.05,uncons.low.new=0.10,uncons.high=0.95,uncons.high.new=0.90,
                    use.last.votes=T,
                    simnum=102,simdepth=simnum, #all first-round options. Produces object of class "judgem".

                    prelim=T,weight="constant", #all second-round options. Takes judgem, makes prelimmed judgem
                    
                    routine=NULL,year=NULL,judgem.object=NULL,
                    predict=F,
                    new.predictors=NULL,  #how to handle this one now?
                    extras=NULL, #extra districts with determined results.
                    Evar=F,
                    distselect=NULL, #subset?
                    voterange=c(0.45,0.55),
                    voteshares=seq(min(voterange),max(voterange),length.out=round((max(voterange)-min(voterange))*100)+1), #options.
                    voteorshift=NULL,winvote=0.5,
                    ...    #for the modelling thing.
                   )

  {

  #if (voteform==~1) stop("I told you not to use that voter formula. Didn't you read the manual?")
    
  if (is.null(judgem.object)) {
    yy <- xx <- wts <- wtv <- wtt <- NULL
    a.n <- function(...) as.numeric(...)
    if (!is.data.frame(data)) { #more than one year contained in frame? If so...
    
     #if (is.null(same.districts)) same.districts <- rep(1,length(data))
     years <- names(data)
     for (ii in 1:length(data)) {
      framer <- model.frame(formula=modelform,data=data[[ii]],na.action=na.pass,...)
      #writeLines("debug 1")
      yy[[ii]] <- a.n(model.response(framer))
      xx[[ii]] <- model.preds(framer)
      if (!is.null(voteform)) {
        fr2 <- model.frame(formula=voteform,data=data[[ii]],na.action=na.pass,...)
        if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
          vt <- matrix(rep(1,length(yy[[ii]])))
        #writeLines("debug 2")
        if (!is.null(vt)) {  #at least one column
        if (dim(vt)[2]>1) {wtv[[ii]] <- a.n(vt[,1]); wtt[[ii]] <- a.n(vt[,2])} else
          wtv[[ii]] <- wtt[[ii]] <- a.n(vt[,1]) }
        st <- model.preds(fr2); wts[[ii]] <- a.n(st[,1])
      }
     }
   
    } else { #only one year this time.
     framer <- model.frame(formula=modelform,data=data,na.action=na.pass,...)
     #writeLines("debug 3")
     years <- "Only year"
     yy[[1]] <- a.n(model.response(framer))
     xx[[1]] <- model.preds(framer)
     if (!is.null(voteform)) {
      fr2 <- model.frame(formula=voteform,data=data,na.action=na.pass,...)
      #writeLines("debug 4")
      if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
        vt <- matrix(rep(1,length(yy[[1]])))
      #writeLines("debug 4a")
      
      if (!is.null(vt)) {  #at least one column
       if (dim(vt)[2]>1) {wtv[[1]] <- a.n(vt[,1]); wtt[[1]] <- a.n(vt[,2])} else
        wtv[[1]] <- wtt[[1]] <- a.n(vt[,1]) }
      #writeLines("debug 5")

      st <- model.preds(fr2); wts[[1]] <- a.n(st[,1])
     }
   }
  
   #all attached at this point.
   test <<- list(xx=xx,yy=yy,v=wtv,t=wtt,s=wts)
   #return(test)
   judgem.object <- judgem.setdata(xx,yy,wtv,wtt,wts,
                   same.districts=same.districts,uncons.method=uncons.method,
                   prelim=prelim)
   judgem.object$years <- years
  }  #this concludes the formulation of the judgem.object.

  #we now have a judgem.object. now, prelim if we're running a routine.

  if ((prelim)&(is.null(judgem.object$lambda))) judgem.object <- prelim(judgem.object)

  if (!is.null(routine)) {
    if (is.null(judgem.object$lambda)) judgem.object <- prelim(judgem.object)
    if (is.na(judgem.object$lambda)) {
      judgem.object$lambda <- 0.5 #what to do if no information? what would Bayes do?
      writeLines ("Lambda is undefined, and is therefore estimated as 0.5.")
    }  
      
    if (is.null(year)) {writeLines (paste("No year was given: the most recent, year ",length(judgem.object$voteshare),", will be used.",sep=""))
                        year <- length(judgem.object$voteshare)}

    if (year>length(judgem.object$voteshare)) stop("The year you have selected is not in the system.")
    
    if (!is.null(new.predictors)) {  #implement modificatins as requested
      if (!is.list(new.predictors)) stop ("Counterfactual predictor
task list must be of class 'list'.")
      if (as.logical(length(new.predictors)%%2)) stop ("Counterfactual
predictor task terms must come in pairs.")
      covarsnew <- judgem.object$covars[[year]]
      for (ii in 1:floor(length(new.predictors)/2))
        covarsnew[,new.predictors[[2*ii-1]] ] <- new.predictors[[2*ii]]
        
    } else covarsnew <- NULL

    judgem.object$covarsnew <- new.list(length(judgem.object$covars))
    judgem.object$covarsnew[[year]] <- covarsnew
    
    try({judgem.object$output <- judgem.core(routine=routine,datacaddy=judgem.object,
                                     yearnum=year,predict=predict,
                                     covarsnew=covarsnew,

                                     extras=NULL, #extra districts with determined results.
                                     Evar=Evar,
                                     distselect=distselect, #subset?
                                             voterange=voterange,voteshares=voteshares, #options.
                                             voteorshift=voteorshift,winvote=winvote)
       judgem.object$outputclass <- routine
       judgem.object$outputyear <- year}) #formula based?
  }
  
  judgem.object
}

