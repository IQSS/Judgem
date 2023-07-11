"uncons.default" <-
function (data,i) {
  data$voteshare[[i]] <- repunc(data$voteshare[[i]],data$uncL,data$uncU,data$uncLR,data$uncUR)
  return(data)
}

