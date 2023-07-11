"summary.judgem" <-
function (object,yearnum=NA,...) {
  qsum <- function (set) c(mean(set[!is.na(set)]),
                           sd(set[!is.na(set)]),
                           var(set[!is.na(set)]),
                           min(set[!is.na(set)]),
                           max(set[!is.na(set)]),
                           sum(1*!is.na(set)),
                           sum(1*is.na(set)))
  #x,y,wtv,elgvotes,seatsper
  out <- NULL
  if (!is.na(yearnum)) {
    out <- rbind(t(apply(object$covars[[yearnum]],2,qsum)),qsum(object$voteshare[[yearnum]]),
               qsum(object$actvotes[[yearnum]]),qsum(object$elgvotes[[yearnum]]),qsum(object$seatsper[[yearnum]]))

    jj <- dim(out)[1]
    rownames(out)[(jj-3):jj] <- c("Vote Proportion",
                                "Votes Cast","Eligible Voters",
                                "Seats per District")
    colnames(out) <- c("Mean","SD","Variance","Min","Max","Valid","Missing")
  } else {
    writeLines(paste("This Judgem object contains data for",length(object$covars),"elections."))
    writeLines (paste("Lambda = ",object$lambda,sep=""))
    writeLines (paste("Sigma = ",object$sigma,sep=""))
  }
  if (!is.null(out)) return(out)
}

