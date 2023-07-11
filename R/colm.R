"colm" <-
function (matrx,index) {
  out <- as.matrix(matrx[,index])
  colnames(out) <- index
  out
}

