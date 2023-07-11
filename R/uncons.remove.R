"uncons.remove" <-
function (data,i) {
  data$voteshare[[i]] <- delunc(data$voteshare[[i]],data$uncL,data$uncU)
  to.keep <- as.numeric(!is.na(data$voteshare[[i]]))*(1:length(data$voteshare[[i]]))
  to.keep <- to.keep[to.keep>0]

  data$voteshare[[i]] <- rowm(data$voteshare[[i]],to.keep)
  data$covars[[i]] <- rowm(data$covars[[i]],to.keep)
  data$actvotes[[i]] <- rowm(data$actvotes[[i]],to.keep)
  data$elgvotes[[i]] <- rowm(data$elgvotes[[i]],to.keep)
  data$seatsper[[i]] <- rowm(data$seatsper[[i]],to.keep)
  
  return(data)
}

