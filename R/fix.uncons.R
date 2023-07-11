"fix.uncons" <-
function (data,same.districts,uncons.method) {
  dists <- length(data$covars)
  for (ii in 1:dists) {
#    writeLines ("uncons debug")
    data <- switch (uncons.method,
          remove = uncons.remove(data,ii),
          impute = uncons.impute(data,ii),
          default = uncons.default(data,ii),
          nochange = data)
#    if (as.logical(same.districts[ii])&use.last.results) data$covars[[ii+1]] <- cbind (data$covars[[ii+1]],data$voteshare[[ii]])
  }

  #Now all five variables are in place. Put them in matrix form. Again.
  for (i in 1:length(data$voteshare)) {
    data$voteshare[[i]] <- as.matrix(data$voteshare[[i]])
    data$covars[[i]] <- as.matrix(data$covars[[i]])
    data$elgvotes[[i]] <- as.matrix(data$elgvotes[[i]])
    data$actvotes[[i]] <- as.matrix(data$actvotes[[i]])
    data$seatsper[[i]] <- as.matrix(data$seatsper[[i]])
  }
  
  data
}

