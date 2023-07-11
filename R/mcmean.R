"mcmean" <-
function (mat,col) matrix(apply(mat,2,weighted.mean,col),1)

