"prelim" <-
function (data,weight="constant") {

  weighttype <- c("constant","actvotes","seats","elgvotes")
  if (!any(weight==weighttype)) stop("Problem in preliminary analysis: unknown choice for district weights. Please select 'constant', 'actvotes', 'elgvotes' or 'seats'.") else 
    distweights <- switch(weight,
               "constant" = {tw <- new.list(length(data$covars));
                             for (ii in 1:length(data$covars)) tw[[ii]] <- array(1,c(dim(data$covars[[ii]])[1],1)); tw},
               "actvotes" = data$actvotes,
               "elgvotes" = data$elgvotes,
               "seats" = data$seats
             ) 

  #writeLines (weight)
  
  data$distweights <- distweights
  data$weight <- weight
  nyears <- length(data$covars)
  #id.prev <- judgem.simtotal
  redist <- data$sam
  z <- data
  z$beta <- z$vc <- list(NA)
  z$sind <- z$lind <- NULL
  for (ii in 1:nyears) {
    #writeLines (paste(ii))
    ury <- data$fullrow[[ii]]
    zt <- reg(data$covars[[ii]][ury,],
              matrix(data$voteshare[[ii]][ury]),
              data$distweights[[ii]][ury])
    lambda <- NA
    #writeLines (paste(ii,"."))
    if (ii<nyears) if (redist[ii+1]) {
      ur <- intersect(data$fullrow[[ii]],data$fullrow[[ii+1]])
      #debug <<- ur
      #debug3 <<- u.c(data$voteshare[[i]],data$covars[[i]],data$covars[[i+1]])
      zt2 <- reg(u.c(data$voteshare[[ii]],data$covars[[ii]],data$covars[[ii+1]])[ur,],
                 matrix(data$voteshare[[ii+1]][ur]),
                 data$distweights[[ii]][ur])
      lambda <- as.numeric(zt2$beta[1])
#    writeLines ("prelim debug")
      
    }
    z$beta[[ii]] <- zt$beta
    z$vc[[ii]] <- zt$vc
    z$sind[ii] <- sqrt(zt$sig2)
    z$lind[ii] <- lambda
    #writeLines (paste(ii,".."))

  }

  good.sigma <- z$sind[!is.na(z$sind)]
  z$sigma <- sqrt(sum(good.sigma^2)/length(good.sigma))
  z$lambda <- mean(z$lind[!is.na(z$lind)])
  z$simbank <- NULL
  return(z)
}

