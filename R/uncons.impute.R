"uncons.impute" <-
function(data,i) {
  data$voteshare[[i]] <- delunc(data$voteshare[[i]],data$uncL,data$uncU)
  data$voteshare[[i]] <- impute(data$covars[[i]],data$voteshare[[i]],data$fullrow[[i]])
  return(data)
}

