"judgem.core" <-
function (routine,datacaddy,yearnum,predict=F,covarsnew=NULL,
                    extras=NULL,Evar=F,distselect=NULL,
                    voterange=c(0.45,0.55),voteshares=8:12/20,
                    voteorshift=NULL,winvote=0.5) {

#op1=NULL,op2=NULL,op3=NULL,
  
  if (is.null(distselect)) distselect <- rep(T,length(datacaddy$voteshare[[yearnum]])) else
    if (length(distselect)!=length(datacaddy$voteshare[[yearnum]])) stop("Your district selection vector doesn't have the right number of districts.")
  
  datacaddy$predict <- predict
  datacaddy$svEvar <- Evar
  datacaddy$covarsnew <- new.list(length(datacaddy$covars))
  if (!is.null(covarsnew)) datacaddy$covarsnew[[yearnum]] <- covarsnew else datacaddy$covarsnew <- datacaddy$covars
  
  if (!is.null(extras)) {
    if (!is.data.frame(extras)) {
      writeLines ("Error in extra districts: not a data frame. Will be ignored.")
      extras <- NULL
    } else
    if (dim(extras)[2]!=3) {
      writeLines ("Error in extra districts: incorrect format. Will be ignored.")
      extras <- NULL
    } else
  
    extras2 <- switch (datacaddy$weight,
                    "constant" = rep(1,dim(extras)[1]),
                    "elgvotes" = extras[,2],
                    "actvotes" = extras[,2],
                    "seats" = extras[,3])

  } else extras2 <- NULL

  datacaddy$extras <- cbind(extras,extras2)
  
  output <- "Execution failure." #should anything go wrong, this is the function's output.

  #year-length trouble here.
  if (any(dim(datacaddy$covars[[yearnum]])!=dim(datacaddy$covarsnew[[yearnum]]))) {
    writeLines (paste("Warning: in year ",yearnum,", old and new covariates have different dimensions. Proceeding without using old covariates.",sep=""))
    datacaddy$covarsnew[[yearnum]] <- datacaddy$covars[[yearnum]]
  }

  #compress and remove those rows with missing values.
  tbo <- cbind (datacaddy$covars[[yearnum]],datacaddy$voteshare[[yearnum]],
                datacaddy$actvotes[[yearnum]],datacaddy$elgvotes[[yearnum]],
                datacaddy$seatsper[[yearnum]],datacaddy$covarsnew[[yearnum]],
                datacaddy$distweights[[yearnum]])

  inn <- missed(tbo)
  datacaddy$covars[[yearnum]] <- rowm(datacaddy$covars[[yearnum]],inn)
  datacaddy$voteshare[[yearnum]] <- rowm(datacaddy$voteshare[[yearnum]],inn)
  datacaddy$actvotes[[yearnum]] <- rowm(datacaddy$actvotes[[yearnum]],inn)
  datacaddy$elgvotes[[yearnum]] <- rowm(datacaddy$elgvotes[[yearnum]],inn)
  datacaddy$seatsper[[yearnum]] <- rowm(datacaddy$seatsper[[yearnum]],inn)
  datacaddy$distweights[[yearnum]] <- rowm(datacaddy$distweights[[yearnum]],inn)
  distselect <- distselect[inn]
  
  if (any(!is.na(datacaddy$covarsnew[[yearnum]]))) datacaddy$covarsnew[[yearnum]] <- rowm(datacaddy$covarsnew[[yearnum]],inn)

  #better.date <- function() {
  #  s <- date()
  #  paste(substr(s,1,3),substr(s,5,7),substr(s,9,10),substr(s,12,13),substr(s,15,16),substr(s,18,19),substr(s,21,24),sep="")
  #}

  output <- switch (routine,
          #"svcurve" = svcurve (datacaddy,yearnum,lohi=voterange),
          #"pvcurve" = pvcurve (datacaddy,yearnum,lohi=voterange),
          "seats" = sv(datacaddy,yearnum,voteshares),
          "prob" = {
            if (is.null(voteorshift)) {
              writeLines ("Quantity voteorshift not specified; setting a 'vote' of 0.45 to 0.55.")
              #writeLines (paste("Vote range is ",voterange[1]," to ",voterange[2],".",sep=""))
              voteorshift <- "vote"
              voteshares <- seq(0.45,0.55,by=0.01)
            }
            if (voteorshift=="vote") pv(datacaddy,yearnum,Ev=voteshares,lohi=voterange,condition=distselect) else
              if (voteorshift=="shift") pv(datacaddy,yearnum,delta=voteshares,lohi=voterange,condition=distselect) else
                stop ("Please choose either 'vote' or 'shift' as your option in 'voteorshift'.")}, 
          #"freq" = freq(datacaddy,yearnum,voterange),
          "conditional.seats" = freq (datacaddy,yearnum,voterange,distselect),
          
          "svsum" = svsum(datacaddy,yearnum),
          "distreport" = dists(datacaddy,yearnum), #,plotornot=T),

          #"votepower" = voting.power(datacaddy,yearnum,op1,op2),
          "winvote" = winvote(datacaddy,yearnum,winvote),
          "winprob" = {
            if (is.null(voteorshift)) winprob(datacaddy,yearnum,voterange) else
             if (voteorshift=="vote") winprob(datacaddy,yearnum,voterange,c(NA,voteshares)) else 
              if (voteorshift=="shift") winprob(datacaddy,yearnum,voterange,c(voteshares,NA)) else
                stop ("Please choose either 'vote' or 'shift' as your option in 'voteorshift'.")
            },
          writeLines ("An invalid routine was entered.")
          )
  
 return(output)
}

